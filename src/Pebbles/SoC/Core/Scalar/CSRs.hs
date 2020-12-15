-- Custom CSRs
module Pebbles.SoC.Core.Scalar.CSRs where

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream

-- Pebbles imports
import Pebbles.Pipeline.Scalar
import Pebbles.Instructions.CSRUnit

-- +-------------+---------+--------+-------------------------------------+
-- | CSR         | Address | Access | Description                         |
-- +-------------+---------+--------+-------------------------------------+
-- | SimEmit     | 0x800   | W      | Emit word in simulation             |
-- | SimFinish   | 0x801   | W      | Terminate simulator                 |
-- | UARTCanPut  | 0x802   | R      | Can write to UART?                  |
-- | UARTPut     | 0x803   | W      | Write byte to UART                  |
-- | UARTCanGet  | 0x804   | R      | Can read from UART?                 |
-- | UARTGet     | 0x805   | R      | Read byte from UART                 |
-- | InstrAddr   | 0x806   | W      | Set instruction mem address         |
-- | WriteInstr  | 0x807   | W      | Write to instruction mem            |
-- +-------------+---------+--------+-------------------------------------+

-- | Writing the this CSR emits a word during simulation
csr_SimEmit :: CSR
csr_SimEmit =
  CSR {
    csrId = 0x800
  , csrRead = Nothing
  , csrWrite = Just \x -> display "0x%08x" x
  }

-- | Writing to this CSR terminates the simulator
csr_SimFinish :: CSR
csr_SimFinish =
  CSR {
    csrId = 0x801
  , csrRead = Nothing
  , csrWrite = Just \x -> finish
  }

-- | CSRs for built-in UART
makeCSRs_UART :: Stream (Bit 8) -> Module ([CSR], Stream (Bit 8))
makeCSRs_UART uartIn = do
  -- UART output buffer
  uartOut :: Queue (Bit 8) <- makeShiftQueue 1

  -- Check if we can write to the UART
  let csr_UARTCanPut =
        CSR {
          csrId = 0x802
        , csrRead = Just do return (uartOut.notFull.zeroExtend)
        , csrWrite = Nothing
        }

  -- Write to the UART
  let csr_UARTPut =
        CSR {
          csrId = 0x803
        , csrRead = Nothing
        , csrWrite = Just \x -> enq uartOut (x.truncate)
        }

  -- Check if we can read from the UART
  let csr_UARTCanGet =
        CSR {
          csrId = 0x804
        , csrRead = Just do return (uartIn.canPeek.zeroExtend)
        , csrWrite = Nothing
        }

  -- Read from the UART
  let csr_UARTGet =
        CSR {
          csrId = 0x805
        , csrRead = Just do
           uartIn.consume
           return (uartIn.peek.zeroExtend)
        , csrWrite = Nothing
        }

  let csrs =
        [ csr_UARTCanPut
        , csr_UARTPut
        , csr_UARTCanGet
        , csr_UARTGet
        ]

  return (csrs, uartOut.toStream)

-- | CSRs for instruction memory access
makeCSRs_InstrMem :: ScalarPipeline -> Module [CSR]
makeCSRs_InstrMem pipe = do
  -- Address for instruction memory write
  addrReg :: Reg (Bit 32) <- makeReg dontCare

  -- Set address for instruction memory write
  let csr_InstrAddr =
        CSR {
          csrId = 0x806
        , csrRead = Nothing
        , csrWrite = Just \x -> do
            addrReg <== x
        }

  -- Write to instruction memory
  let csr_WriteInstr =
        CSR {
          csrId = 0x807
        , csrRead = Nothing
        , csrWrite = Just \x -> do
            writeInstr pipe (addrReg.val) x
        }

  return [csr_InstrAddr, csr_WriteInstr]
