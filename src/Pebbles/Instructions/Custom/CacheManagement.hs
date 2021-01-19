module Pebbles.Instructions.Custom.CacheManagement where

-- Custom instructions cache management

-- Blarney imports
import Blarney
import Blarney.BitScan
import Blarney.SourceSink

-- Pebbles imports
import Pebbles.Pipeline.Interface
import Pebbles.Memory.Interface

-- Decode stage
-- ============

decodeCacheMgmt =
  [ "0000000 rs2<5> rs1<5> 000 rd<5> 0001000" --> "CACHE_FLUSH_LINE"
  ]

-- Execute stage
-- =============

executeCacheMgmt :: MemUnit InstrInfo -> DecodeInfo -> State -> Action ()
executeCacheMgmt memUnit d s = do
  -- Cache line flush
  when (d.opcode `is` ["CACHE_FLUSH_LINE"]) do
    if memUnit.memReqs.canPut
      then do
        -- Send request to memory unit
        put (memUnit.memReqs)
          MemReq {
            memReqId = dontCare
          , memReqAccessWidth = dontCare
          , memReqOp = memCacheFlushOp
          , memReqAddr = s.opA
          , memReqData = dontCare
          , memReqIsUnsigned = dontCare
          }
      else s.retry
