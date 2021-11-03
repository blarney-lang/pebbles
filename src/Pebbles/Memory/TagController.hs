module Pebbles.Memory.TagController
  ( makeTagController
  , makeNullTagController
  , TagCtrlReqId
  ) where

-- SoC parameters
#include <Config.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.SourceSink
import Blarney.Connectable
import qualified Blarney.Vector as V

-- Pebbles imports
import Pebbles.Util.Counter
import Pebbles.Memory.TagCache
import Pebbles.Memory.DRAM.Bus
import Pebbles.Memory.DRAM.Interface

-- Types
-- =====

-- | Request id for DRAM requests from tag controller
type TagCtrlReqId t_id = DRAMBusId (StreamCacheReqId, t_id)

-- Implementation
-- ==============

-- | Tag controller
makeTagController :: Bits t_id =>
     Stream (DRAMReq t_id)
     -- ^ Requests to tag controller
  -> Stream (DRAMResp (TagCtrlReqId t_id))
     -- ^ Responses from DRAM
  -> Module (Stream (DRAMResp t_id), Stream (DRAMReq (TagCtrlReqId t_id)))
     -- ^ Responses from tag controller, and requests to DRAM
makeTagController reqs dramResps = mdo
  -- Request queue for tag cache
  tagReqQueue <- makeQueue

  -- Request queue for DRAM
  dramReqQueue <- makePipelineQueue 1

  -- Response queue for data
  dataRespQueue :: Queue (DRAMResp t_id) <-
    makeSizedQueue DRAMLogMaxInFlight

  -- In-flight data request counter
  inFlightCount :: Counter (DRAMLogMaxInFlight+1) <-
    makeCounter (fromInteger (2^DRAMLogMaxInFlight))

  -- Count beats in a burst
  burstCount :: Reg DRAMBurst <- makeReg 0
  let burstCountNew = burstCount.val + 1

  -- Helper functions to modify request/response id
  let modifyReqId f = fmap (\req -> req { dramReqId = f req.dramReqId })
  let modifyRespId f = fmap (\resp -> resp { dramRespId = f resp.dramRespId })

  -- Tag cache
  (tagResps, dramReqs0) <- makeBoundary "TagCache" makeTagCache
    (toStream tagReqQueue) dramResps0'

  -- Data requests
  let dramReqs1 = toStream dramReqQueue

  -- Combine ids
  let dramReqs0' = modifyReqId (\id -> (id, dontCare)) dramReqs0
  let dramReqs1' = modifyReqId (\id -> (dontCare, id)) dramReqs1

  -- DRAM bus
  ((dramResps0, dramResps1), dramReqs) <-
    makeDRAMBus (dramReqs0', dramReqs1') dramResps

  -- Split ids
  let dramResps0' = modifyRespId fst dramResps0
  let dramResps1' = modifyRespId snd dramResps1

  -- Connect data response queue
  makeConnection dramResps1' (toSink dataRespQueue)

  -- Split requests to tag cache and DRAM request queue
  always do
    let req = reqs.peek
    when (reqs.canPeek .&&. inFlightCount.getAvailable .>=.
                              zeroExtend req.dramReqBurst) do
      -- DRAM address, accounting for bursts
      let addr = req.dramReqAddr + zeroExtend burstCount.val
      -- Determine the address of the beat containing the desired tag bits,
      -- and their offset within that beat
      let (tagsBase, tagsOffset) = split addr
      -- Formulate request for tag cache
      let tagReq =
            StreamCacheReq {
              streamCacheReqId = ()
            , streamCacheReqIsStore = req.dramReqIsStore
            , streamCacheReqAddr = zeroExtend tagsBase
            , streamCacheReqOffset = tagsOffset
            , streamCacheReqData = req.dramReqDataTagBits
            , streamCacheReqWriteEn =
                pack $ V.map V.or $ unpack $ req.dramReqByteEn
            }
      -- Issue tag and data requests
      when (tagReqQueue.notFull .&&. dramReqQueue.notFull) do
        let burstDone = burstCountNew .==. req.dramReqBurst
        -- Consume request
        when (burstDone .||. req.dramReqIsStore) do
          reqs.consume
        -- Increment inflight count
        when (burstDone .&&. inv req.dramReqIsStore) do
          inFlightCount `incrBy` zeroExtend (req.dramReqBurst)
        -- Update burst count
        if burstDone
          then burstCount <== 0
          else burstCount <== burstCountNew
        -- Issue data request
        if req.dramReqIsStore
          then do
            -- Burst stores get debursted here
            -- See Note [Tag Controller Deburst]
            enq dramReqQueue
              req {
                dramReqAddr = addr
              , dramReqBurst = 1
              , dramReqIsFinal = true
              }
          else do
            when (burstCount.val .==. 0) do
              enq dramReqQueue req
        -- Issue tag request
        enq tagReqQueue tagReq

  let resps =
        Source {
          peek =
            DRAMResp {
              dramRespId = dataRespQueue.first.dramRespId
            , dramRespBurstId = dataRespQueue.first.dramRespBurstId
            , dramRespData = dataRespQueue.first.dramRespData
            , dramRespDataTagBits = tagResps.peek.streamCacheRespData
            }
        , canPeek = dataRespQueue.canDeq .&&. tagResps.canPeek
        , consume = do
            tagResps.consume
            dataRespQueue.deq
            inFlightCount `decrBy` 1
        }

  return (resps, dramReqs)

-- | Null tag controller. Same interface as `makeTagController` but
-- does not care about tags.
makeNullTagController :: Bits t_id =>
     Stream (DRAMReq t_id)
     -- ^ Requests to tag controller
  -> Stream (DRAMResp (TagCtrlReqId t_id))
     -- ^ Responses from DRAM
  -> Module (Stream (DRAMResp t_id), Stream (DRAMReq (TagCtrlReqId t_id)))
     -- ^ Responses from tag controller, and requests to DRAM
makeNullTagController reqs resps = do
  -- Helper functions to modify request/response id
  let modifyReqId f = fmap (\req -> req { dramReqId = f req.dramReqId })
  let modifyRespId f = fmap (\resp -> resp { dramRespId = f resp.dramRespId })
 
  -- Add and remove ids
  let reqs' = modifyReqId (\id -> (dontCare, (dontCare, id))) reqs
  let resps' = modifyRespId (\id -> snd (snd id)) resps

  return (resps', reqs')

-- Note [Tag Controller Deburst]
-- =============================

-- It's important to deburst stores in the tag controller, otherwise
-- the following deadlock can occur: (1) the tag controller forwards
-- the first flit of a burst to DRAM; (2) the DRAM bus between the tag
-- cache and the data stream becomes locked until the burst is
-- complete; (3) the tag cache becomes blocked while handling a miss
-- and cannot proceed due to (2); (4) the tag controller waits for
-- *both* the tag cache *and* the data stream to become available
-- before it can forward the next flit of the burst, which never
-- happens due to (3).  This debursting process has the side effect of
-- dropping atomicity on multi-request transactions; however, this is
-- fine because accesses to tag and data memory are non-overlapping
-- and fundamentally independent.
