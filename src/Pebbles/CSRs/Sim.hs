-- Custom CSRs for controlling simulation
module Pebbles.CSRs.Sim where

-- Blarney imports
import Blarney

-- Pebbles imports
import Pebbles.CSRs.CSRUnit

-- +-------------+---------+--------+-------------------------------------+
-- | CSR         | Address | Access | Description                         |
-- +-------------+---------+--------+-------------------------------------+
-- | SimEmit     | 0x800   | W      | Emit word in simulation             |
-- | SimFinish   | 0x801   | W      | Terminate simulator                 |
-- +-------------+---------+--------+-------------------------------------+

-- | Writing the this CSR emits a word during simulation
csr_SimEmit :: CSR
csr_SimEmit =
  CSR {
    csrId = 0x800
  , csrRead = Nothing
  , csrWrite = Just \x -> display "0x" (formatHex 8 x)
  }

-- | Writing to this CSR terminates the simulator
csr_SimFinish :: CSR
csr_SimFinish =
  CSR {
    csrId = 0x801
  , csrRead = Nothing
  , csrWrite = Just \x -> finish
  }

-- | All CSRs defined in this file
csrs_Sim :: [CSR]
csrs_Sim = [csr_SimEmit, csr_SimFinish]
