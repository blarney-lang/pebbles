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

-- | Bus tag indicates client id of the original request
-- (Maximum of 8 clients)
type DRAMBusId t_id = (Bit 3, t_id)

-- N-way DRAM bus
-- ==============

-- | Create a multi-port DRAM wrapper around a single DRAM.
-- Requests are merged fairly and responses are returned in order.
makeDRAMBus :: forall t_id. Bits t_id =>
     [Stream (DRAMReq t_id)]
     -- ^ Client request streams
  -> Stream (DRAMResp (DRAMBusId t_id))
     -- ^ Responses from DRAM
  -> Module ([Stream (DRAMResp t_id)],
             Stream (DRAMReq (DRAMBusId t_id)))
     -- ^ Client response streams, and requests to DRAM
makeDRAMBus reqStreams dramResps = do
  let numClients = length reqStreams
  staticAssert (numClients >= 1 && numClients <= 8)
    "makeDRAMBus: number of clients must be in [1..8]"

  -- Tag a request with a client id
  let tag t req = req { dramReqId = (t, req.dramReqId) }

  -- Tag the request in each stream with a client id
  let dramReqStreams = [ fmap (tag (fromInteger i)) s
                       | (s, i) <- zip reqStreams [0..] ]

  -- Fair merger
  dramReqs <- treeM1 (\rs0 rs1 ->
    makeGenericFairMergeTwo makeQueue (const true)
                            dramReqIsFinal (rs0, rs1)) dramReqStreams

  -- Get the tag from the response
  let getTag resp = fst resp.dramRespId

  -- Untag the response
  let untag resp = resp { dramRespId = snd resp.dramRespId }

  -- Split into response streams
  let respStreams =
        [ Source {
            peek = untag dramResps.peek
          , canPeek = dramResps.canPeek .&&.
                        getTag dramResps.peek .==. fromIntegral i
          , consume = dramResps.consume
          }
        | i <- [0 .. numClients - 1] ]
 
  return (respStreams, dramReqs)
