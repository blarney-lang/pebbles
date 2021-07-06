-- 32-bit scalar core with 5-stage pipeline

module Core.Scalar where

-- Blarney imports
import Blarney
import Blarney.Stream
import Blarney.SourceSink
import Blarney.Interconnect

-- Pebbles imports
import Pebbles.CSRs.Trap
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
import Pebbles.Memory.DRAM.Interface

-- | Configuration parameters
data ScalarCoreConfig =
  ScalarCoreConfig {
    scalarCoreInstrMemInitFile :: Maybe String
    -- ^ Initialisation file for instruction memory
  , scalarCoreInstrMemLogNumInstrs :: Int
    -- ^ Size of tightly coupled instruction memory
  , scalarCoreInitialPC :: Integer
    -- ^ Initial PC
  }

-- | Scalar core inputs
data ScalarCoreIns =
  ScalarCoreIns {
    scalarUartIn :: Stream (Bit 8)
    -- ^ UART input
  , scalarMemUnit :: MemUnit InstrInfo
    -- ^ Memory unit
  , scalarSIMTResps :: Stream SIMTResp
    -- ^ Management responses from SIMT core
  } deriving (Generic, Interface)

-- | Scalar core outputs
data ScalarCoreOuts =
  ScalarCoreOuts {
    scalarUartOut :: Stream (Bit 8)
    -- ^ UART output
  , scalarSIMTReqs :: Stream SIMTReq
    -- ^ Management requests to SIMT core
  } deriving (Generic, Interface)

-- | RV32IM core with UART input and output channels
makeScalarCore ::
     ScalarCoreConfig
     -- ^ Configuration parameters
  -> ScalarCoreIns
     -- ^ Scalar core inputs
  -> Module ScalarCoreOuts
     -- ^ Scalar core outputs
makeScalarCore config inputs = mdo
  -- UART CSRs
  (uartCSRs, uartOut) <- makeCSRs_UART (inputs.scalarUartIn)

  -- Instruction memory CSRs
  imemCSRs <- makeCSRs_InstrMem (pipeline.writeInstr)

  -- SIMT management CSRs
  (simtReqs, simtCSRs) <- makeCSRs_SIMTHost (inputs.scalarSIMTResps)

  -- Cycle count CSRs
  cycleCSRs <- makeCSR_CycleCount

  -- Trap CSRs
  (trapCSRs, trapRegs) <- makeCSRs_Trap

  -- CSR unit
  csrUnit <- makeCSRUnit $
       csrs_Sim
    ++ uartCSRs
    ++ imemCSRs
    ++ simtCSRs
    ++ cycleCSRs
    ++ trapCSRs
 
  -- Multiplier
  mulUnit <- makeHalfMulUnit

  -- Divider
  divUnit <- makeSeqDivUnit

  -- Processor pipeline
  pipeline <- makeScalarPipeline
    ScalarPipelineConfig {
      instrMemInitFile = config.scalarCoreInstrMemInitFile
    , instrMemLogNumInstrs = config.scalarCoreInstrMemLogNumInstrs
    , initialPC = config.scalarCoreInitialPC
    , decodeStage = decodeI ++ decodeM ++ decodeCacheMgmt
    , executeStage = \s -> return
        ExecuteStage {
          execute = do
            executeI Nothing csrUnit (inputs.scalarMemUnit) s
            executeM mulUnit divUnit s
            executeCacheMgmt (inputs.scalarMemUnit) s
        , resumeReqs = mergeTree
            [ fmap memRespToResumeReq (inputs.scalarMemUnit.memResps)
            , mulUnit.mulResps
            , divUnit.divResps
            ]
        }
    , trapCSRs = trapRegs
    }

  return
    ScalarCoreOuts {
      scalarUartOut = uartOut
    , scalarSIMTReqs = simtReqs
    }
