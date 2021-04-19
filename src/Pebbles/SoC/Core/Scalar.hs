-- 32-bit scalar core with 5-stage pipeline

module Pebbles.SoC.Core.Scalar where

-- Blarney imports
import Blarney
import Blarney.Stream
import Blarney.SourceSink
import Blarney.Interconnect

-- Pebbles imports
import Pebbles.CSRs.CSRUnit
import Pebbles.CSRs.CycleCount
import Pebbles.CSRs.Custom.Simulate
import Pebbles.CSRs.Custom.UART
import Pebbles.CSRs.Custom.InstrMem
import Pebbles.CSRs.Custom.SIMTHost
import Pebbles.Pipeline.Scalar
import Pebbles.Pipeline.Interface
import Pebbles.Pipeline.SIMT.Management
import Pebbles.Instructions.RV32_I
import Pebbles.Instructions.RV32_M
import Pebbles.Instructions.Units.MulUnit
import Pebbles.Instructions.Units.DivUnit
import Pebbles.Instructions.Custom.CacheManagement
import Pebbles.Memory.Interface
import Pebbles.SoC.DRAM.Interface

-- | Configuration parameters
data ScalarCoreConfig =
  ScalarCoreConfig {
    -- | Initialisation file for instruction memory
    scalarCoreInstrMemInitFile :: Maybe String
    -- | Size of tightly coupled instruction memory
  , scalarCoreInstrMemLogNumInstrs :: Int
  }

-- | Scalar core inputs
data ScalarCoreIns =
  ScalarCoreIns {
    -- | UART input
    scalarUartIn :: Stream (Bit 8)
    -- | Memory unit
  , scalarMemUnit :: MemUnit InstrInfo
    -- | Management responses from SIMT core
  , scalarSIMTResps :: Stream SIMTResp
  }

-- | Scalar core outputs
data ScalarCoreOuts =
  ScalarCoreOuts {
    -- | UART output
    scalarUartOut :: Stream (Bit 8)
    -- | Management requests to SIMT core
  , scalarSIMTReqs :: Stream SIMTReq
  }

-- | RV32IM core with UART input and output channels
makeScalarCore ::
     -- | Configuration parameters
     ScalarCoreConfig
     -- | Scalar core inputs
  -> ScalarCoreIns
     -- | Scalar core outputs
  -> Module ScalarCoreOuts
makeScalarCore config inputs = mdo
  -- UART CSRs
  (uartCSRs, uartOut) <- makeCSRs_UART (inputs.scalarUartIn)

  -- Instruction memory CSRs
  imemCSRs <- makeCSRs_InstrMem (pipeline.writeInstr)

  -- SIMT management CSRs
  (simtReqs, simtCSRs) <- makeCSRs_SIMTHost (inputs.scalarSIMTResps)

  -- Cycle count CSRs
  cycleCSRs <- makeCSR_CycleCount

  -- CSR unit
  csrUnit <- makeCSRUnit $
       csrs_Sim
    ++ uartCSRs
    ++ imemCSRs
    ++ simtCSRs
    ++ cycleCSRs
 
  -- Multiplier
  mulUnit <- makeHalfMulUnit

  -- Divider
  divUnit <- makeSeqDivUnit

  -- Processor pipeline
  pipeline <- makeScalarPipeline
    ScalarPipelineConfig {
      instrMemInitFile = config.scalarCoreInstrMemInitFile
    , instrMemLogNumInstrs = config.scalarCoreInstrMemLogNumInstrs
    , decodeStage = decodeI ++ decodeM ++ decodeCacheMgmt
    , executeStage = \s -> return
        ExecuteStage {
          execute = do
            executeI csrUnit (inputs.scalarMemUnit) s
            executeM mulUnit divUnit s
            executeCacheMgmt (inputs.scalarMemUnit) s
        , resumeReqs = mergeTree
            [ fmap memRespToResumeReq (inputs.scalarMemUnit.memResps)
            , mulUnit.mulResps
            , divUnit.divResps
            ]
        }
    }

  return
    ScalarCoreOuts {
      scalarUartOut = uartOut
    , scalarSIMTReqs = simtReqs
    }
