module Pebbles.Instructions.Custom.SIMT where

-- Custom instructions for SIMT thread convergence

-- Blarney imports
import Blarney
import Blarney.BitScan

-- Pebbles imports
import Pebbles.Instructions.Mnemonics

-- Decode stage
-- ============

decodeSIMT =
  [ "0000000 <5> <5> 000 <5> 0001001" --> SIMT_PUSH
  , "0000000 <5> <5> 001 <5> 0001001" --> SIMT_POP
  ]
