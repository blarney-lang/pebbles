module Pebbles.Instructions.Custom.FastZeroing where

-- Custom instructions for fast zeroing

-- Blarney imports
import Blarney
import Blarney.BitScan
import Blarney.SourceSink

-- Pebbles imports
import Pebbles.Memory.Interface
import Pebbles.Pipeline.Interface
import Pebbles.Instructions.Mnemonics

-- Decode stage
-- ============

decodeFastZeroing =
  [ "0000001 rs2<5> rs1<5> 000 <5> 0001000" --> FAST_ZERO
  ]

-- Execute stage
-- =============

executeFastZeroing :: Sink MemReq -> State -> Action ()
executeFastZeroing memReqs s = do
  -- Cache line flush
  when (s.opcode `is` [FAST_ZERO]) do
    if memReqs.canPut
      then do
        -- Send request to memory unit
        put memReqs
          MemReq {
            memReqAccessWidth = dontCare
          , memReqOp = memZeroOp
          , memReqAMOInfo = dontCare
          , memReqAddr = s.opA
          , memReqData = s.opB
          , memReqDataTagBit = dontCare
          , memReqDataTagBitMask = dontCare
          , memReqIsUnsigned = dontCare
          , memReqIsFinal = true
          }
      else s.retry
