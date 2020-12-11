-- 32-bit scalar core with 5-stage pipeline and data cache

module Pebbles.SoC.Core.Scalar where

-- Blarney imports
import Blarney
import Blarney.Stream
import Blarney.SourceSink
import Blarney.Interconnect

-- Pebbles imports
import Pebbles.Memory.Interface
import Pebbles.Pipeline.Scalar
import Pebbles.Pipeline.Interface
import Pebbles.SoC.DRAM.Interface
import Pebbles.Instructions.RV32_I
import Pebbles.Instructions.RV32_M
import Pebbles.Instructions.CSRUnit
import Pebbles.Instructions.MulUnit
import Pebbles.Instructions.DivUnit

-- Scalar core custom CSRs
import Pebbles.SoC.Core.Scalar.CSRs

-- | Configuration parameters
data ScalarCoreConfig =
  ScalarCoreConfig {
    -- | Initialisation file for instruction memory
    scalarCoreInstrMemInitFile :: Maybe String
    -- | Size of tightly coupled instruction memory
  , scalarCoreInstrMemLogNumInstrs :: Int
  }

-- | RV32IM core with UART input and output channels
makeScalarCore ::
     -- | Configuration parameters
     ScalarCoreConfig
     -- | UART input
  -> Stream (Bit 8)
     -- | Memory unit
  -> MemUnit InstrInfo
     -- | UART output
  -> Module (Stream (Bit 8))
makeScalarCore config uartIn memUnit = mdo
  -- UART CSRs
  (uartCSRs, uartOut) <- makeCSRs_UART uartIn

  -- Instruction memory CSRs
  imemCSRs <- makeCSRs_InstrMem pipe

  -- CSR unit
  csrUnit <- makeCSRUnit $
       [csr_SimEmit, csr_SimFinish]
    ++ uartCSRs
    ++ imemCSRs
 
  -- Multiplier
  mulUnit <- makeHalfMulUnit

  -- Divider
  divUnit <- makeSeqDivUnit

  -- Processor pipeline
  pipe <- makeScalarPipeline
    ScalarPipelineConfig {
      instrMemInitFile = config.scalarCoreInstrMemInitFile
    , instrMemLogNumInstrs = config.scalarCoreInstrMemLogNumInstrs
    , decodeStage = decodeI ++ decodeM
    , executeStage = \s -> do
        executeI csrUnit memUnit s
        executeM mulUnit divUnit s
    , resumeStage = mergeTree
        [ fmap memRespToResumeReq (memUnit.memResps)
        , mulUnit.mulResps
        , divUnit.divResps
        ]
    }

  return uartOut

-- | Convert memory response to pipeline resume request
memRespToResumeReq :: MemResp InstrInfo -> ResumeReq
memRespToResumeReq resp =
  ResumeReq {
    resumeReqInfo = resp.memRespId
  , resumeReqData = resp.memRespData
  }
