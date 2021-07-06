-- Standard CSRs for traps
module Pebbles.CSRs.Trap where

-- Blarney imports
import Blarney

-- Pebbles imports
import Pebbles.CSRs.CSRUnit
import Pebbles.CSRs.TrapCodes

-- +-------------+---------+--------+-------------------------------------+
-- | CSR         | Address | Access | Description                         |
-- +-------------+---------+--------+-------------------------------------+
-- | mepc        |   0x341 |      R | Machine exception program counter   |
-- | mcause      |   0x342 |      R | Machine trap cause                  |
-- | mtvec       |   0x305 |      W | Machine trap-handler base address   |
-- +-------------+---------+--------+-------------------------------------+

-- | Trap CSRs
data TrapCSRs =
  TrapCSRs {
    csr_mepc :: Reg (Bit 32)
  , csr_mcause :: Reg (Bit 32)
  , csr_mtvec :: Reg (Bit 32)
  }

-- | Create trap-related CSRs
makeCSRs_Trap :: Module ([CSR], TrapCSRs)
makeCSRs_Trap = do
  -- Machine exception program counter
  mepc :: Reg (Bit 32) <- makeReg 0

  -- Machine trap-handler base address
  mtvec :: Reg (Bit 32) <- makeReg 0

  -- Machine trap cause
  mcause :: Reg (Bit 32) <- makeReg 0

  let csrs =
        [ -- mepc
          CSR {
            csrId = 0x341
          , csrRead = Just do return (mepc.val)
          , csrWrite = Nothing
          }
          -- mcause
        , CSR {
            csrId = 0x342
          , csrRead = Just do return (mcause.val)
          , csrWrite = Nothing
          }
          -- mtvec
        , CSR {
            csrId = 0x305
          , csrRead = Nothing
          , csrWrite = Just do \x -> mtvec <== x
         }
       ]

  let regs =
        TrapCSRs {
          csr_mepc = mepc
        , csr_mcause = mcause
        , csr_mtvec  = mtvec
        }

  return (csrs, regs)
