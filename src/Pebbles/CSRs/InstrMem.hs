-- Custom CSRs for initialising CPU tightly-coupled instruction memory
module Pebbles.CSRs.InstrMem where

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream

-- Pebbles imports
import Pebbles.CSRs.CSRUnit

-- +-------------+---------+--------+-------------------------------------+
-- | CSR         | Address | Access | Description                         |
-- +-------------+---------+--------+-------------------------------------+
-- | InstrAddr   | 0x806   | W      | Set instruction mem address         |
-- | WriteInstr  | 0x807   | W      | Write to instruction mem            |
-- +-------------+---------+--------+-------------------------------------+

-- | Method to write to instruction memory
type WriteInstrMethod = Bit 32 -> Bit 32 -> Action ()

-- | CSRs for instruction memory access
makeCSRs_InstrMem :: WriteInstrMethod -> Module [CSR]
makeCSRs_InstrMem writeInstr = do
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
            writeInstr (addrReg.val) x
        }

  return [csr_InstrAddr, csr_WriteInstr]
