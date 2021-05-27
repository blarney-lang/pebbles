module Pebbles.Memory.DRAM.DualPort
  ( makeDRAMDualPort
  ) where

-- SoC parameters
#include <Config.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.Interconnect

-- Pebbles imports
import Pebbles.Memory.DRAM.Wrapper
import Pebbles.Memory.DRAM.Interface

-- Types
-- =====

type Dual a = (a, a)

-- Dual-port DRAM
-- ==============

-- | Create a dual-port DRAM wrapper around a single DRAM instance.
-- Requests are merged fairly and responses are returned in order.
makeDRAMDualPort :: forall t_id. Bits t_id =>
     -- | DRAM request streams coming in
     Dual (Stream (DRAMReq t_id))
     -- | Avalon DRAM input signals
  -> AvalonDRAMIns
     -- | DRAM response streams, and Avalon DRAM output signals
  -> Module (Dual (Stream (DRAMResp t_id)), AvalonDRAMOuts)
makeDRAMDualPort (reqs0, reqs1) avlIns = do
  -- Tag a request with a client id
  let tag t req = req { dramReqId = (t, req.dramReqId) }

  -- Tag the request in each stream with a client id
  let dramReqs0 = fmap (tag 0) reqs0
  let dramReqs1 = fmap (tag 1) reqs1

  -- Fair merger
  dramReqs <- makeGenericFairMergeTwo makeQueue (const true)
                dramReqIsFinal (dramReqs0, dramReqs1)

  -- Single DRAM instance
  (dramResps, avlOuts) <- makeDRAM dramReqs avlIns

  -- Get the tag from the response
  let getTag resp = resp.dramRespId.fst

  -- Untag the response
  let untag resp = resp { dramRespId = resp.dramRespId.snd }

  -- Split into two response streams
  let resps0 =
        Source {
          peek = dramResps.peek.untag
        , canPeek = dramResps.canPeek .&. dramResps.peek.getTag.inv
        , consume = dramResps.consume
        }
  let resps1 =
        Source {
          peek = dramResps.peek.untag
        , canPeek = dramResps.canPeek .&. dramResps.peek.getTag
        , consume = dramResps.consume
        }

  return ((resps0, resps1), avlOuts)
