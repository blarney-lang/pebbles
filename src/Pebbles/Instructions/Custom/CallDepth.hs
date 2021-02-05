module Pebbles.Instructions.Custom.CallDepth where

-- Logic for tracking function call depth

-- Blarney imports
import Blarney
import Blarney.BitScan
import Blarney.PulseWire
import Blarney.SourceSink

-- Pebbles imports
import Pebbles.Util.Counter
import Pebbles.Pipeline.Interface

-- Decode stage
-- ============

decodeCallDepth =
  [ -- JAL instruction with ra as link register 
    "imm[20] imm[10:1] imm[11] imm[19:12] 00001 1101111" --> "CALL_DEPTH_INC"
    -- JALR instruction with ra as link register
  , "imm[11:0] rs1<5> 000 00001 1100111" --> "CALL_DEPTH_INC"
    -- JALR instruction with ra as target register
  , "imm[11:0] 00001 000 rd<5> 1100111" --> "CALL_DEPTH_DEC"
  ]

-- Execute stage
-- =============

executeCallDepth ::
     -- | Pulse this to increment call depth
     PulseWire
     -- | Pulse this to decrement call depth
  -> PulseWire
     -- | Result of decoder
  -> DecodeInfo
     -- | Pipeline state
  -> State
  -> Action ()
executeCallDepth incWire decWire d s = do
  -- Increment call depth
  when (d.opcode `is` ["CALL_DEPTH_INC"]) do
    incWire.pulse

  -- Decrement call depth
  when (d.opcode `is` ["CALL_DEPTH_DEC"]) do
    decWire.pulse
