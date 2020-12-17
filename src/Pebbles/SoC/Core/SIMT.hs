-- 32-bit SIMT core

module Pebbles.SoC.Core.SIMT where

-- SoC configuration
#include <SoC.h>

-- Blarney imports
import Blarney
import Blarney.Stream
import Blarney.SourceSink
import Blarney.Interconnect

-- Pebbles imports
import Pebbles.CSRs.Sim
import Pebbles.CSRs.Hart
import Pebbles.CSRs.CSRUnit
import Pebbles.Memory.Interface
import Pebbles.Pipeline.SIMT
import Pebbles.Pipeline.Interface
import Pebbles.SoC.DRAM.Interface
import Pebbles.Instructions.RV32_I
import Pebbles.Instructions.RV32_M
import Pebbles.Instructions.MulUnit
import Pebbles.Instructions.DivUnit

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
     -- | Memory unit per vector lane
  -> [MemUnit InstrInfo]
     -- | UART output
  -> Module SIMTPipeline
makeSIMTCore config memUnits = mdo
  -- Sanity check
  staticAssert (length memUnits == SIMTLanes)
    "makeSIMTCore: number of memory units doesn't match number of lanes"

  -- CSR unit per vector lane
  csrUnits <- sequence
    [ do let laneId :: Bit SIMTLogLanes = fromInteger i
         let hartId = truncate (pipeline.currentWarpId # laneId)
         makeCSRUnit $
              csrs_Sim
           ++ [csr_HartId hartId]
    | i <- [0..SIMTLanes-1] ]
 
  -- Multiplier per vector lane
  mulUnits <- replicateM SIMTLanes makeHalfMulUnit

  -- Divider per vector lane
  divUnits <- replicateM SIMTLanes makeSeqDivUnit

  -- Processor pipeline
  pipeline <- makeSIMTPipeline
    SIMTPipelineConfig {
      instrMemInitFile = config.simtCoreInstrMemInitFile
    , instrMemLogNumInstrs = config.simtCoreInstrMemLogNumInstrs
    , logNumWarps = SIMTLogWarps
    , decodeStage = decodeI ++ decodeM
    , executeStage =
        [ \s -> do
            executeI csrUnit memUnit s
            executeM mulUnit divUnit s
        | (memUnit, mulUnit, divUnit, csrUnit) <-
            zip4 memUnits mulUnits divUnits csrUnits ]
    , resumeStage =
        [ mergeTree
            [ fmap memRespToResumeReq (memUnit.memResps)
            , mulUnit.mulResps
            , divUnit.divResps
            ]
        | (memUnit, mulUnit, divUnit) <-
            zip3 memUnits mulUnits divUnits ]
    }

  return pipeline
