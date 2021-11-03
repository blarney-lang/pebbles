module Pebbles.Memory.DRAM.Bus
  ( makeDRAMBus
  , DRAMBusId
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

-- | Bus tag indicates client id of the original request
type DRAMBusId t_id = (Bit 1, t_id)

-- Two-way DRAM bus
-- ================

-- Client A <--> +----------+    +------+
--               | DRAM Bus |<-->| DRAM |
-- Client B <--> +----------+    +------+

-- | Create a dual-port DRAM wrapper around a single DRAM.
-- Requests are merged fairly and responses are returned in order.
makeDRAMBus :: forall t_id. Bits t_id =>
     Dual (Stream (DRAMReq t_id))
     -- ^ Client request streams
  -> Stream (DRAMResp (DRAMBusId t_id))
     -- ^ Responses from DRAM
  -> Module (Dual (Stream (DRAMResp t_id)),
             Stream (DRAMReq (DRAMBusId t_id)))
     -- ^ Client response streams, and requests to DRAM
makeDRAMBus (reqs0, reqs1) dramResps = do
  -- Tag a request with a client id
  let tag t req = req { dramReqId = (t, req.dramReqId) }

  -- Tag the request in each stream with a client id
  let dramReqs0 = fmap (tag 0) reqs0
  let dramReqs1 = fmap (tag 1) reqs1

  -- Fair merger
  dramReqs <- makeGenericFairMergeTwo makeQueue (const true)
                dramReqIsFinal (dramReqs0, dramReqs1)

  -- Get the tag from the response
  let getTag resp = fst resp.dramRespId

  -- Untag the response
  let untag resp = resp { dramRespId = snd resp.dramRespId }

  -- Split into two response streams
  let resps0 =
        Source {
          peek = untag dramResps.peek
        , canPeek = dramResps.canPeek .&. inv (getTag dramResps.peek)
        , consume = dramResps.consume
        }
  let resps1 =
        Source {
          peek = untag dramResps.peek
        , canPeek = dramResps.canPeek .&. getTag dramResps.peek
        , consume = dramResps.consume
        }

  return ((resps0, resps1), dramReqs)
