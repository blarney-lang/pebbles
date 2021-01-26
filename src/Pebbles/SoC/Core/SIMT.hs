-- 32-bit SIMT core

module Pebbles.SoC.Core.SIMT where

-- SoC configuration
#include <SoC.h>

-- Blarney imports
import Blarney
import Blarney.Stream
import Blarney.PulseWire
import Blarney.SourceSink
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

-- | Configuration parameters
data SIMTCoreConfig =
  SIMTCoreConfig {
    -- | Initialisation file for instruction memory
    simtCoreInstrMemInitFile :: Maybe String
    -- | Size of tightly coupled instruction memory
  , simtCoreInstrMemLogNumInstrs :: Int
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

  -- SIMT warp control CSRs
  (warpTermWire, csrs_WarpControl) <-
    makeCSRs_WarpControl (pipelineOuts.simtKernelAddr)

  -- CSR unit per vector lane
  csrUnits <- sequence
    [ do let laneId :: Bit SIMTLogLanes = fromInteger i
         let hartId = truncate (pipelineOuts.simtCurrentWarpId # laneId)
         makeCSRUnit $
              csrs_Sim
           ++ [csr_HartId hartId]
           ++ (if i == 0 then csrs_WarpControl else [])
    | i <- [0..SIMTLanes-1] ]
 
  -- Multiplier per vector lane
  mulUnits <- replicateM SIMTLanes makeHalfMulUnit

  -- Divider per vector lane
  divUnits <- replicateM SIMTLanes makeSeqDivUnit

  -- Wires for tracking function call depth
  incCallDepth <- makePulseWire
  decCallDepth <- makePulseWire

  -- Pipeline configuration
  let pipelineConfig =
        SIMTPipelineConfig {
          instrMemInitFile = config.simtCoreInstrMemInitFile
        , instrMemLogNumInstrs = config.simtCoreInstrMemLogNumInstrs
        , logNumWarps = SIMTLogWarps
        , logMaxCallDepth = SIMTLogMaxCallDepth
        , decodeStage = decodeI ++ decodeM ++ decodeCallDepth
        , executeStage =
            [ \d s -> do
                executeI csrUnit memUnit d s
                executeM mulUnit divUnit d s
                executeCallDepth incCallDepth decCallDepth d s
            | (memUnit, mulUnit, divUnit, csrUnit) <-
                zip4 memUnits' mulUnits divUnits csrUnits ]
       , resumeStage =
            [ mergeTree
                [ fmap memRespToResumeReq (memUnit.memResps)
                , mulUnit.mulResps
                , divUnit.divResps
               ]
            | (memUnit, mulUnit, divUnit) <-
                zip3 memUnits' mulUnits divUnits ]
        }

  -- Pipeline instantiation
  pipelineOuts <- makeSIMTPipeline pipelineConfig
    SIMTPipelineIns {
      simtMgmtReqs = mgmtReqs
    , simtWarpTerminatedWire = warpTermWire
    , simtIncCallDepth = incCallDepth.val
    , simtDecCallDepth = decCallDepth.val
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
