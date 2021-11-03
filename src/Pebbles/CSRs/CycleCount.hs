-- Standard cycle count CSRs
module Pebbles.CSRs.CycleCount where

-- Blarney imports
import Blarney

-- Pebbles imports
import Pebbles.CSRs.CSRUnit

-- +-------------+---------+--------+-------------------------------------+
-- | CSR         | Address | Access | Description                         |
-- +-------------+---------+--------+-------------------------------------+
-- | Cycle       | 0xc00   | R      | Cycle count (lower 32 bits)         |
-- | CycleH      | 0xc80   | R      | Cycle count (upper 32 bits)         |
-- +-------------+---------+--------+-------------------------------------+

makeCSR_CycleCount :: Module [CSR]
makeCSR_CycleCount = do
  -- Cycle counter
  count :: Reg (Bit 64) <- makeReg 0

  always do
    count <== count.val + 1

  return
    [ CSR {
        csrId = 0xc00
      , csrRead = Just do return (lower count.val)
      , csrWrite = Nothing
      }
    , CSR {
        csrId = 0xc80
      , csrRead = Just do return (upper count.val)
      , csrWrite = Nothing
      }
    ]
