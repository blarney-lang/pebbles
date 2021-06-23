module Pebbles.Memory.DRAM.Deburster
  ( makeDRAMDeburster
  ) where

-- Blarney imports
import Blarney
import Blarney.Stream

-- Pebbles imports
import Pebbles.Memory.DRAM.Interface

-- | Module to remove bursts from DRAM requests
makeDRAMDeburster ::
     Stream (DRAMReq t_id)
     -- ^ Burst requests
  -> Module (Stream (DRAMReq t_id))
     -- ^ Debursted requests
makeDRAMDeburster reqs = do
  -- Beat counter for bursts
  burstCount :: Reg DRAMBurst <- makeReg 0
  let burstCountNew = burstCount.val + 1

  -- Debursted output stream
  return
    Source {
      peek =
        (reqs.peek) {
          dramReqAddr = reqs.peek.dramReqAddr +
            burstCount.val.zeroExtend
        , dramReqBurst = 1
        }
    , canPeek = reqs.canPeek
    , consume = do
        if burstCountNew .==. reqs.peek.dramReqBurst
          then do
            burstCount <== 0
            -- Only consume load when burst complete
            when (reqs.peek.dramReqIsStore.inv) do
              reqs.consume
          else burstCount <== burstCountNew
        -- Always consume a store
        when (reqs.peek.dramReqIsStore) do
          reqs.consume
    }
