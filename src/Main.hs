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

-- RV32I core with UART input and output channels
makePebbles :: Bool -> Stream (Bit 8) -> Module (Stream (Bit 8))
makePebbles sim uartIn = mdo
  -- CSR unit
  (uartOut, csrUnit) <- makeCSRUnit uartIn

  -- Tightly-coupled data memory
  memUnit <- makeDTCM sim

  -- Multiplier
  mulUnit <- makeHalfMulUnit

  -- Divider
  divUnit <- makeSeqDivUnit

  -- Processor pipeline
  makePipeline sim 
    Config {
      decodeStage = decodeI ++ decodeM
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
simPebbles :: Module ()
simPebbles = do
  uartOut <- makePebbles True nullStream
  always do
    when (uartOut.canPeek) do
      display_ "%c" (uartOut.peek)
      uartOut.consume

main :: IO ()
main = do
  writeVerilogModule simPebbles "SimPebbles" "verilog/"
  writeVerilogModule (makePebbles False) "Pebbles" "verilog/"
