-- 32-bit SIMT core

module Pebbles.SoC.Core.SIMT where

-- SoC configuration
#include <SoC.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.PulseWire
import Blarney.SourceSink
import Blarney.Connectable
import Blarney.Interconnect

-- Pebbles imports
import Pebbles.CSRs.Hart
import Pebbles.CSRs.CSRUnit
import Pebbles.CSRs.Custom.Simulate
import Pebbles.CSRs.Custom.WarpControl
import Pebbles.Util.Counter
import Pebbles.Memory.Interface
import Pebbles.Pipeline.SIMT
import Pebbles.Pipeline.SIMT.Management
import Pebbles.Pipeline.Interface
import Pebbles.SoC.DRAM.Interface
import Pebbles.Instructions.RV32_I
import Pebbles.Instructions.RV32_M
import Pebbles.Instructions.MulUnit
import Pebbles.Instructions.DivUnit
import Pebbles.Instructions.Custom.CallDepth

-- Haskell imports
import Data.List

-- Execute stage
-- =============

-- | SIMT execute stage inputs
data SIMTExecuteIns =
  SIMTExecuteIns {
    -- | Lane id
    execLaneId :: Bit SIMTLogLanes
    -- | Warp id
  , execWarpId :: Bit SIMTLogWarps
    -- | Kernel address
  , execKernelAddr :: Bit 32
    -- | Wire to trigger warp termination
  , execWarpTerm :: Wire (Bit 1)
    -- | Pulse wire for call depth increment
  , execCallDepthInc :: PulseWire
    -- | Pulse wire for call depth decrement
  , execCallDepthDec :: PulseWire
    -- | Memory unit interface for lane
  , execMemUnit :: MemUnit InstrInfo
  } deriving (Generic, Interface)

-- | Execute stage for a SIMT lane (synthesis boundary)
makeSIMTExecuteStage :: SIMTExecuteIns -> State -> Module ExecuteStage
makeSIMTExecuteStage ins s = do
  -- Multiplier per vector lane
  mulUnit <- makeFullMulUnit

  -- Divider per vector lane
  divUnit <- makeSeqDivUnit

  -- SIMT warp control CSRs
  csr_WarpTerm <- makeCSR_WarpTerminate (ins.execLaneId) (ins.execWarpTerm)
  csr_WarpGetKernel <- makeCSR_WarpGetKernel (ins.execKernelAddr)

  -- CSR unit
  let hartId = zeroExtend (ins.execWarpId # ins.execLaneId)
  csrUnit <- makeCSRUnit $
       csrs_Sim
    ++ [csr_HartId hartId]
    ++ [csr_WarpTerm]
    ++ [csr_WarpGetKernel]
 
  -- Merge resume requests
  let resumeReqStream =
        mergeTree
          [ fmap memRespToResumeReq (ins.execMemUnit.memResps)
          , mulUnit.mulResps
          , divUnit.divResps
          ]

  -- Resume queue
  resumeQueue <- makePipelineQueue 1
  makeConnection resumeReqStream (resumeQueue.toSink)

  return
    ExecuteStage {
      execute = do
        executeI csrUnit (ins.execMemUnit) s
        executeM mulUnit divUnit s
        executeCallDepth (ins.execCallDepthInc) (ins.execCallDepthDec) s
    , resumeReqs = resumeQueue.toStream
    }

-- | Generate verilog for execute stage
genSIMTExecuteStage :: String -> IO ()
genSIMTExecuteStage dir =
  writeVerilogModule makeSIMTExecuteStage "SIMTExecuteStage" dir

-- Core
-- ====

-- | Configuration parameters
data SIMTCoreConfig =
  SIMTCoreConfig {
    -- | Initialisation file for instruction memory
    simtCoreInstrMemInitFile :: Maybe String
    -- | Size of tightly coupled instruction memory
  , simtCoreInstrMemLogNumInstrs :: Int
    -- | Synthesis boundary on execute stage?
  , simtCoreExecBoundary :: Bool
  }

-- | RV32IM SIMT core
makeSIMTCore ::
     -- | Configuration parameters
     SIMTCoreConfig
     -- | SIMT management requests
  -> Stream SIMTReq
     -- | Memory unit per vector lane
  -> [MemUnit InstrInfo]
     -- | SIMT management responses
  -> Module (Stream SIMTResp)
makeSIMTCore config mgmtReqs memUnits = mdo
  -- Sanity check
  staticAssert (length memUnits == SIMTLanes)
    "makeSIMTCore: number of memory units doesn't match number of lanes"

  -- Apply stack address interleaving
  let memUnits' = interleaveStacks memUnits

  -- Wires for tracking function call depth
  incCallDepths <- replicateM SIMTLanes makePulseWire
  decCallDepths <- replicateM SIMTLanes makePulseWire

  -- Wire for warp termination
  warpTermWire :: Wire (Bit 1) <- makeWire 0

  -- Synthesis boundary on execute stage?
  let exec = if config.simtCoreExecBoundary
               then makeInstanceWithTypeOf makeSIMTExecuteStage
                      "SIMTExecuteStage"
               else makeSIMTExecuteStage

  -- Pipeline configuration
  let pipelineConfig =
        SIMTPipelineConfig {
          instrMemInitFile = config.simtCoreInstrMemInitFile
        , instrMemLogNumInstrs = config.simtCoreInstrMemLogNumInstrs
        , logNumWarps = SIMTLogWarps
        , logMaxCallDepth = SIMTLogMaxCallDepth
        , decodeStage = decodeI ++ decodeM ++ decodeCallDepth
        , executeStage =
            [ exec
                SIMTExecuteIns {
                  execLaneId = fromInteger i
                , execWarpId = pipelineOuts.simtCurrentWarpId.truncate
                , execKernelAddr = pipelineOuts.simtKernelAddr
                , execWarpTerm = warpTermWire
                , execCallDepthInc = incCD
                , execCallDepthDec = decCD
                , execMemUnit = memUnit
                }
            | (memUnit, incCD, decCD, i) <-
                zip4 memUnits' incCallDepths decCallDepths [0..] ]
        }

  -- Pipeline instantiation
  pipelineOuts <- makeSIMTPipeline pipelineConfig
    SIMTPipelineIns {
      simtMgmtReqs = mgmtReqs
    , simtWarpTerminatedWire = warpTermWire
    , simtIncCallDepth = map val incCallDepths
    , simtDecCallDepth = map val decCallDepths
    }

  return (pipelineOuts.simtMgmtResps)

-- | Stack address interleaver so that accesses to same stack
-- offset by different threads in a warp are coalesced
interleaveStacks :: [MemUnit id] -> [MemUnit id]
interleaveStacks memUnits =
    [ memUnit {
        memReqs = mapSink interleaveReq (memUnit.memReqs)
      }
    | memUnit <- memUnits]
  where
    interleaveReq :: MemReq id -> MemReq id
    interleaveReq req = req { memReqAddr = interleaveAddr (req.memReqAddr) }

    interleaveAddr :: Bit 32 -> Bit 32
    interleaveAddr a =
      if top .==. ones
        then top # stackOffset # stackId # wordOffset
        else a
      where
        top = slice @31 @(SIMTLogWarps+SIMTLogLanes+SIMTLogBytesPerStack) a
        stackId = slice @(SIMTLogWarps+SIMTLogLanes+SIMTLogBytesPerStack-1)
                        @SIMTLogBytesPerStack a
        stackOffset = slice @(SIMTLogBytesPerStack-1) @2 a
        wordOffset = slice @1 @0 a
