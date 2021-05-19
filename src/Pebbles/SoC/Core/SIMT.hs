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
import Blarney.Vector (Vec, toList)

-- Pebbles imports
import Pebbles.CSRs.Hart
import Pebbles.CSRs.CSRUnit
import Pebbles.CSRs.Custom.Simulate
import Pebbles.CSRs.Custom.SIMTDevice
import Pebbles.Util.Counter
import Pebbles.Memory.Interface
import Pebbles.Pipeline.SIMT
import Pebbles.Pipeline.SIMT.Management
import Pebbles.Pipeline.Interface
import Pebbles.SoC.DRAM.Interface
import Pebbles.Instructions.RV32_I
import Pebbles.Instructions.RV32_M
import Pebbles.Instructions.RV32_A
import Pebbles.Instructions.Mnemonics
import Pebbles.Instructions.Units.MulUnit
import Pebbles.Instructions.Units.DivUnit
import Pebbles.Instructions.Custom.SIMT

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
    -- | Wire containing warp command
  , execWarpCmd :: Wire WarpCmd
    -- | Memory unit interface for lane
  , execMemUnit :: MemUnit InstrInfo
  } deriving (Generic, Interface)

-- | Execute stage for a SIMT lane (synthesis boundary)
makeSIMTExecuteStage :: SIMTExecuteIns -> State -> Module ExecuteStage
makeSIMTExecuteStage = makeBoundary "SIMTExecuteStage" \ins s -> do
  -- Multiplier per vector lane
  mulUnit <- makeFullMulUnit

  -- Divider per vector lane
  divUnit <- makeSeqDivUnit

  -- SIMT warp control CSRs
  csr_WarpCmd <- makeCSR_WarpCmd (ins.execLaneId) (ins.execWarpCmd)
  csr_WarpGetKernel <- makeCSR_WarpGetKernel (ins.execKernelAddr)

  -- CSR unit
  let hartId = zeroExtend (ins.execWarpId # ins.execLaneId)
  csrUnit <- makeCSRUnit $
       csrs_Sim
    ++ [csr_HartId hartId]
    ++ [csr_WarpCmd]
    ++ [csr_WarpGetKernel]
 
  -- Merge resume requests
  let resumeReqStream =
        fmap memRespToResumeReq (ins.execMemUnit.memResps)
          `mergeTwo` mergeTwo (mulUnit.mulResps) (divUnit.divResps)

  -- Resume queue
  resumeQueue <- makePipelineQueue 1
  makeConnection resumeReqStream (resumeQueue.toSink)

  return
    ExecuteStage {
      execute = do
        executeI csrUnit (ins.execMemUnit) s
        executeM mulUnit divUnit s
        executeA (ins.execMemUnit) s
    , resumeReqs = resumeQueue.toStream
    }

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
  -> Vec SIMTLanes (MemUnit InstrInfo)
     -- | SIMT management responses
  -> Module (Stream SIMTResp)
makeSIMTCore config mgmtReqs memUnitsVec = mdo
  let memUnits = toList memUnitsVec

  -- Apply stack address interleaving
  let memUnits' = interleaveStacks memUnits

  -- Wire for warp command
  warpCmdWire :: Wire WarpCmd <- makeWire dontCare

  -- Pipeline configuration
  let pipelineConfig =
        SIMTPipelineConfig {
          instrMemInitFile = config.simtCoreInstrMemInitFile
        , instrMemLogNumInstrs = config.simtCoreInstrMemLogNumInstrs
        , logNumWarps = SIMTLogWarps
        , logMaxNestLevel = SIMTLogMaxNestLevel
        , enableStatCounters = SIMTEnableStatCounters == 1
        , decodeStage = decodeI ++ decodeM ++ decodeA ++ decodeSIMT
        , executeStage =
            [ makeSIMTExecuteStage
                SIMTExecuteIns {
                  execLaneId = fromInteger i
                , execWarpId = pipelineOuts.simtCurrentWarpId.truncate
                , execKernelAddr = pipelineOuts.simtKernelAddr
                , execWarpCmd = warpCmdWire
                , execMemUnit = memUnit
                }
            | (memUnit, i) <- zip memUnits' [0..] ]
        , simtPushTag = SIMT_PUSH
        , simtPopTag = SIMT_POP
        }

  -- Pipeline instantiation
  pipelineOuts <- makeSIMTPipeline pipelineConfig
    SIMTPipelineIns {
      simtMgmtReqs = mgmtReqs
    , simtWarpCmdWire = warpCmdWire
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
