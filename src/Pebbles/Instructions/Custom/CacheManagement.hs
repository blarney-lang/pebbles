module Pebbles.Instructions.Custom.CacheManagement where

-- Custom instructions cache management

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

decodeCacheMgmt =
  [ "0000000 rs2<5> rs1<5> 000 rd<5> 0001000" --> CACHE_FLUSH_LINE
  ]

-- Execute stage
-- =============

executeCacheMgmt :: Sink MemReq -> State -> Action ()
executeCacheMgmt memReqs s = do
  -- Cache line flush
  when (s.opcode `is` [CACHE_FLUSH_LINE]) do
    if memReqs.canPut
      then do
        -- Send request to memory unit
        put memReqs
          MemReq {
            memReqAccessWidth = dontCare
          , memReqOp = memCacheFlushOp
          , memReqAMOInfo = dontCare
          , memReqAddr = s.opA
          , memReqData = dontCare
          , memReqDataTagBit = 0
          , memReqDataTagBitMask = 0
          , memReqIsUnsigned = dontCare
          , memReqIsFinal = true
          }
      else s.retry
