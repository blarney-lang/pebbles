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
  | CSRRS
  | CSRRC

    -- M extension
  | MUL
  | DIV

    -- A extension
  | AMO

    -- Custom extension (Cache Management)
  | CACHE_FLUSH_LINE

    -- Custom extension (SIMT convergence)
  | SIMT_PUSH
  | SIMT_POP

  deriving (Bounded, Enum, Show, Ord, Eq)

-- | Function for checking if any of the given mnemonics are active
infix 8 `is`
is :: MnemonicVec -> [Mnemonic] -> Bit 1
is vec ms = orList [unsafeAt (fromEnum m) vec | m <- ms]
