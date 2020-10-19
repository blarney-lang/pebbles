module Pebbles.Core.Scalar where

-- 32-bit scalar core with 5-stage pipeline

-- Blarney imports
import Blarney
import Blarney.Stream
import Blarney.SourceSink

-- Pebbles imports
import Pebbles.Memory.Interface
import Pebbles.Memory.DTCM
import Pebbles.Pipeline.Interface
import Pebbles.Pipeline.Scalar
import Pebbles.Instructions.RV32_I
import Pebbles.Instructions.RV32_M
import Pebbles.Instructions.CSRUnit
import Pebbles.Instructions.MulUnit
import Pebbles.Instructions.DivUnit

-- Configuration parameters
data ScalarCoreConfig =
  ScalarCoreConfig {
    scalarCoreInstrMemInitFile :: Maybe String
  , scalarCoreDataMemInitFile  :: Maybe String
  }

-- RV32IM core with UART input and output channels
makeScalarCore :: ScalarCoreConfig -> Stream (Bit 8) -> Module (Stream (Bit 8))
makeScalarCore config uartIn = mdo
  -- CSR unit
  (uartOut, csrUnit) <- makeCSRUnit uartIn

  -- Tightly-coupled data memory
  memUnit <- makeDTCM (config.scalarCoreDataMemInitFile)

  -- Multiplier
  mulUnit <- makeHalfMulUnit

  -- Divider
  divUnit <- makeSeqDivUnit

  -- Processor pipeline
  makeScalarPipeline
    ScalarPipelineConfig {
      instrMemInitFile = config.scalarCoreInstrMemInitFile
    , decodeStage = decodeI ++ decodeM
    , executeStage = \s -> do
        executeI csrUnit memUnit s
        executeM mulUnit divUnit s
    , resumeStage = mergeSourcesTree
        [ fmap memRespToResumeReq (memUnit.memResps)
        , mulUnit.mulResps
        , divUnit.divResps
        ]
    }

  return uartOut

-- Simulation version
makeScalarCoreSim :: ScalarCoreConfig -> Module ()
makeScalarCoreSim config = do
  uartOut <- makeScalarCore config nullStream
  always do
    when (uartOut.canPeek) do
      display_ "%c" (uartOut.peek)
      uartOut.consume
