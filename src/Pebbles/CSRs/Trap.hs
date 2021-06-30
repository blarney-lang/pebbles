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

-- | Trap function
type TrapFun = TrapCode -> Action ()

-- | Create trap-related CSRs
makeCSRs_Trap :: ReadWrite (Bit 32) -> Module ([CSR], TrapFun)
makeCSRs_Trap pc = do
  -- Machine exception program counter
  mepc :: Reg (Bit 32) <- makeReg 0

  -- Machine trap-handler base address
  mtvec :: Reg (Bit 32) <- makeReg 0

  -- Machine trap cause
  mcause :: Reg (Bit 32) <- makeReg 0

  -- Trigger trap
  trapWire :: Wire (Bit 1) <- makeWire false

  -- Common trap logic
  always do
    when (trapWire.val) do
      mepc <== pc.val
      pc <== slice @31 @2 (mtvec.val) # 0b00

  -- Trap function
  let trapFun = \code -> do
        mcause <== code.trapCodeIsInterrupt # code.trapCodeCause
        trapWire <== true

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

  return (csrs, trapFun)
