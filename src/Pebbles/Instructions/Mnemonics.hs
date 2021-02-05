module Pebbles.Instructions.Mnemonics where

-- Blarney imports
import Blarney

-- Pebbles imports
import Pebbles.Pipeline.Interface

data Mnemonic =
    -- I extension
    LUI
  | AUIPC
  | ADD
  | SLT
  | SLTU
  | AND
  | OR
  | XOR
  | SLL
  | SRL
  | SRA
  | SUB
  | JAL
  | JALR
  | BEQ
  | BNE
  | BLT
  | BLTU
  | BGE
  | BGEU
  | LOAD
  | STORE
  | FENCE
  | ECALL
  | EBREAK
  | CSRRW

    -- M extension
  | MUL
  | DIV

    -- Custom extension (Cache Management)
  | CACHE_FLUSH_LINE

    -- Custom extension (Call Depth Tracking)
  | CALL_DEPTH_INC
  | CALL_DEPTH_DEC

  deriving (Bounded, Enum, Show, Ord, Eq)

-- | Function for checking if any of the given mnemonics are active
infix 8 `is`
is :: MnemonicVec -> [Mnemonic] -> Bit 1
is vec ms = orList [unsafeAt (fromEnum m) vec | m <- ms]
