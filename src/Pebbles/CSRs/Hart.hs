-- Standard CSRs for hardware thread (hart) management
module Pebbles.CSRs.Hart where

-- Blarney imports
import Blarney

-- Pebbles imports
import Pebbles.CSRs.CSRUnit

-- +-------------+---------+--------+-------------------------------------+
-- | CSR         | Address | Access | Description                         |
-- +-------------+---------+--------+-------------------------------------+
-- | HartId      | 0xf14   | R      | Get hardware thread id              |
-- +-------------+---------+--------+-------------------------------------+

-- | Writing the this CSR emits a word during simulation
csr_HartId :: Bit 32 -> CSR
csr_HartId id =
  CSR {
    csrId = 0xf14
  , csrRead = Just do return id
  , csrWrite = Nothing
  }
