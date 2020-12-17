-- Custom CSRs for a built-in UART
module Pebbles.CSRs.UART where

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream

-- Pebbles imports
import Pebbles.CSRs.CSRUnit

-- +-------------+---------+--------+-------------------------------------+
-- | CSR         | Address | Access | Description                         |
-- +-------------+---------+--------+-------------------------------------+
-- | UARTCanPut  | 0x802   | R      | Can write to UART?                  |
-- | UARTPut     | 0x803   | W      | Write byte to UART                  |
-- | UARTCanGet  | 0x804   | R      | Can read from UART?                 |
-- | UARTGet     | 0x805   | R      | Read byte from UART                 |
-- +-------------+---------+--------+-------------------------------------+

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
