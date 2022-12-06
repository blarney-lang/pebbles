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
import Blarney.Option
import Blarney.SourceSink
import Blarney.Connectable
import Blarney.TaggedUnion
import qualified Blarney.Vector as V

-- Pebbles imports
import Pebbles.Util.Counter
import Pebbles.Util.SearchQueue
import Pebbles.Memory.DRAM.Bus
import Pebbles.Memory.DRAM.Interface
import Pebbles.Memory.TagCache qualified as TC
import Pebbles.Memory.ZeroCache qualified as ZC

-- Types
-- =====

-- | Request id for DRAM requests from tag controller. Requests can
-- come from tag cache, zero cache, or data access.
type TagCtrlReqId t_id =
  DRAMBusId (
    TaggedUnion
      [ "tagBusId"  ::: TC.StreamCacheReqId
      , "zeroBusId" ::: ZC.StreamCacheReqId
      , "dataBusId" ::: t_id
      ]
  )

-- | Information about a request to the zero cache, needing when
-- processing a response.
data ZeroCacheReqInfo =
  ZeroCacheReqInfo {
    isStore :: Bit 1
    -- Did load request arise from a partial store?
  , addr :: DRAMAddr
    -- ^ Address that led to zero cache lookup
  , byteEn :: DRAMByteEn
    -- ^ Byte enable that led to zero cache lookup
  } deriving (Generic, Bits, Interface)

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
  let enFastZeroing = EnableFastZeroing == 1

  -- Request queue for tag cache
  tagReqQueue <- makeQueue

  -- Request queue for zero cache
  zeroReqQueue <- makeQueue

  -- Request queue for DRAM
  dramReqQueue <- makePipelineQueue 1

  -- Response queue for data
  dataRespQueue :: Queue (DRAMResp t_id) <-
    makeSizedQueueCore DRAMLogMaxInFlight

  -- In-flight data request counter
  inFlightCount :: Counter (DRAMLogMaxInFlight+1) <-
    makeCounter (fromInteger (2^DRAMLogMaxInFlight))

  -- For fast zeroing, we need to block access to certain addresses
  -- temporarily: see Note [Design for Fast Zeroing].
  busyAddrs <- makeSearchQueue False ZeroCacheMaxOutstandingPartialStores

  -- Count beats in a burst
  burstCount :: Reg DRAMBurst <- makeReg 0
  let burstCountNew = burstCount.val + 1

  -- Helper functions to modify request/response id
  let modifyReqId f = fmap (\req -> req { dramReqId = f req.dramReqId })
  let modifyRespId f g src =
        Source {
          canPeek = src.canPeek .&&. g src.peek.dramRespId
        , peek = src.peek { dramRespId = f src.peek.dramRespId }
        , consume = src.consume
        }

  -- Tag cache
  let tagCacheConf =
        TC.StreamCacheConfig { useZeroTable = TagCacheHierarchical /= 0
                             , baseAddr = 0 }
  (tagResps, dramReqs0) <-
    makeBoundary "TagCache" (TC.makeStreamCache tagCacheConf)
      (toStream tagReqQueue) dramResps0'

  -- Data requests
  let dramReqs1 = toStream dramReqQueue

  -- Zero cache
  let zeroCacheConf =
        ZC.StreamCacheConfig {
          useZeroTable = ZeroCacheHierarchical /= 0
        , baseAddr = 2 ^ (DRAMAddrWidth-5)
        }
  (zeroResps, dramReqs2) <-
    if enFastZeroing
      then makeBoundary "ZeroCache" (ZC.makeStreamCache zeroCacheConf)
             (toStream zeroReqQueue) dramResps2'
      else return (nullStream, nullStream)

  -- Combine ids
  let dramReqs0' = modifyReqId (tag #tagBusId) dramReqs0
  let dramReqs1' = modifyReqId (tag #dataBusId) dramReqs1
  let dramReqs2' = modifyReqId (tag #zeroBusId) dramReqs2

  -- DRAM bus
  let dramReqStreams =
        [dramReqs0', dramReqs1'] ++ [dramReqs2' | enFastZeroing]
  (dramRespStreams, dramReqs) <-
    makeDRAMBus dramReqStreams dramResps
  let dramResps0 = dramRespStreams !! 0
  let dramResps1 = dramRespStreams !! 1
  let dramResps2 =
        if enFastZeroing then dramRespStreams !! 2 else nullStream

  -- Split ids
  let dramResps0' = modifyRespId (untag #tagBusId) (`is` #tagBusId) dramResps0
  let dramResps1' = modifyRespId (untag #dataBusId) (const true) dramResps1
  let dramResps2' = modifyRespId (untag #zeroBusId) (const true) dramResps2

  -- Connect data response queue
  makeConnection dramResps1' (toSink dataRespQueue)

  -- Split requests to tag cache and DRAM request queue (and zero cache)
  always do
    -- Try to deal with zero table lookup arising from partial store
    if zeroResps.canPeek .&&. zeroResps.peek.streamCacheRespId.isStore
      then do
        let respInfo = zeroResps.peek.streamCacheRespId
        -- Issue tag and data (and zero) requests
        when (tagReqQueue.notFull .&&. dramReqQueue.notFull
                                  .&&. zeroReqQueue.notFull) do
          -- Wast beat zero?
          when (inv zeroResps.peek.streamCacheRespData) do
            -- Zero out untouched data
            dramReqQueue.enq
              DRAMReq {
                dramReqId = dontCare
              , dramReqIsStore = true
              , dramReqIsFastZero = false
              , dramReqAddr = respInfo.addr
              , dramReqData = 0
              , dramReqDataTagBits = 0
              , dramReqByteEn = inv respInfo.byteEn
              , dramReqBurst = 1
              , dramReqIsFinal = true
              }
            -- Zero out untouched tags
            tagReqQueue.enq
              TC.StreamCacheReq {
                streamCacheReqId = ()
              , streamCacheReqIsStore = true
              , streamCacheReqAddr = upper respInfo.addr
              , streamCacheReqOffset = lower respInfo.addr
              , streamCacheReqData = 0
              , streamCacheReqWriteEn =
                  pack $ V.map V.or $ unpack $ inv respInfo.byteEn
              , streamCacheReqZeroBits = none
              }
          -- Mark beat as non-zero
          zeroReqQueue.enq
            ZC.StreamCacheReq {
              streamCacheReqId = dontCare
            , streamCacheReqIsStore = true
            , streamCacheReqAddr = upper respInfo.addr
            , streamCacheReqOffset = lower respInfo.addr
            , streamCacheReqData = true 
            , streamCacheReqWriteEn = true
            , streamCacheReqZeroBits = none
            }
          zeroResps.consume
          busyAddrs.delete
      else if reqs.canPeek .&&. reqs.peek.dramReqIsFastZero then do
        when enFastZeroing do
          when zeroReqQueue.notFull do
            let (addr, offset) = split reqs.peek.dramReqAddr
            -- Check that address is correctly aligned
            dynamicAssert (offset .==. 0)
              "TagController (Fast Zeroing): address incorrectly aligned"
            -- Mark beats as zero
            zeroReqQueue.enq
              ZC.StreamCacheReq {
                streamCacheReqId = dontCare
              , streamCacheReqIsStore = true
              , streamCacheReqAddr = addr
              , streamCacheReqOffset = 0
              , streamCacheReqData = false
              , streamCacheReqWriteEn = true
              , streamCacheReqZeroBits = some (reqs.peek.dramReqData)
              }
            reqs.consume
      else when (reqs.canPeek .&&.
                   inFlightCount.getAvailable .>=.
                   zeroExtend reqs.peek.dramReqBurst) do
        -- Try to submit requests to DRAM, tag cache, and zero cache
        let req = reqs.peek
        -- DRAM address, accounting for bursts
        let addr = req.dramReqAddr + zeroExtend burstCount.val
        -- If load address is busy, wait
        let wait = if enFastZeroing then busyAddrs.member addr else false
        -- Issue tag and data (and zero) requests
        when (inv wait .&&. tagReqQueue.notFull
                       .&&. dramReqQueue.notFull
                       .&&. zeroReqQueue.notFull
                       .&&. busyAddrs.canInsert) do
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
          tagReqQueue.enq
            TC.StreamCacheReq {
              streamCacheReqId = ()
            , streamCacheReqIsStore = req.dramReqIsStore
            , streamCacheReqAddr = upper addr
            , streamCacheReqOffset = lower addr
            , streamCacheReqData = req.dramReqDataTagBits
            , streamCacheReqWriteEn =
                pack $ V.map V.or $ unpack $ req.dramReqByteEn
            , streamCacheReqZeroBits = none
            }
          -- Issue zero request
          when enFastZeroing do
            -- Is it a full store, i.e. all byte enables set?
            let full = req.dramReqByteEn .==. ones
            let fullStore = req.dramReqIsStore .&&. full
            let partialStore = req.dramReqIsStore .&&. inv full
            -- We want to lookup the zero cache at this point, unless
            -- we're dealing with a full (i.e. non-partial) store, in which
            -- case we can simply mark the beat as non-zero.
            zeroReqQueue.enq
              ZC.StreamCacheReq {
                streamCacheReqId =
                  ZeroCacheReqInfo {
                    isStore = req.dramReqIsStore
                  , addr = addr
                  , byteEn = req.dramReqByteEn
                  }
                -- ^ Use id field to remember load request details
                -- originating from a partial store
              , streamCacheReqIsStore = fullStore
              , streamCacheReqAddr = upper addr
              , streamCacheReqOffset = lower addr
              , streamCacheReqData = true 
                -- ^ If a full store, mark as non-zero
              , streamCacheReqWriteEn = true
              , streamCacheReqZeroBits = none
              }
            -- For a partial store, we need to block loads of the
            -- address until the store has been fully handled
            when partialStore do busyAddrs.insert addr

  let resps =
        Source {
          peek =
            DRAMResp {
              dramRespId = dataRespQueue.first.dramRespId
            , dramRespBurstId = dataRespQueue.first.dramRespBurstId
            , dramRespData =
                if enFastZeroing
                  then zeroResps.peek.streamCacheRespData ?
                         (dataRespQueue.first.dramRespData, 0)
                  else dataRespQueue.first.dramRespData
            , dramRespDataTagBits =
                if enFastZeroing
                  then zeroResps.peek.streamCacheRespData ?
                         (tagResps.peek.streamCacheRespData, 0)
                  else tagResps.peek.streamCacheRespData
            }
        , canPeek = andList $
            [dataRespQueue.canDeq, tagResps.canPeek] ++
              [ zeroResps.canPeek .&&.
                  inv zeroResps.peek.streamCacheRespId.isStore
              | enFastZeroing ]
        , consume = do
            tagResps.consume
            dataRespQueue.deq
            inFlightCount `decrBy` 1
            when enFastZeroing do zeroResps.consume
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
  let reqs' = modifyReqId (\id -> (dontCare, tag #dataBusId id)) reqs
  let resps' = modifyRespId (\id -> untag #dataBusId (snd id)) resps

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

-- Note [Design for Fast Zeroing]
-- ==============================

-- Maintain a zero table in DRAM:
--   * 1 bit for every 512-bit beat (64 bytes) of DRAM
--   * Denotes whether the beat is 0 or not (bit = 0 means beat = 0)
--   * With 4GB of DRAM, there are 2^26 of these bits (8MB table)
--
-- Cache this table onchip:
--   * Cache line is parameterisable, but assuming it is 512 bits,
--     then one cache line covers 32KB of DRAM, i.e. can zero 32KB per
--     clock cycle
--   * Cache is optionally hierarchical: the cache has an option to
--     track which lines are all zero in an onchip table.
--
-- Zeroing algorithm in tag controller:
--   * Loads:
--       - Issue load to DRAM and zero cache in parallel
--       - On load response, mask out data if known zero
--   * Full store (i.e. all bytes being written):
--       - Forward store to DRAM and mark beat as non-zero, in parallel
--   * Partial store (i.e. not all bytes being written):
--       - Need to write zeros for bytes not covered by store
--       - Forward store to DRAM and lookup zero cache in parallel
--       - Put address of store in a blacklist
--       - Loads of blacklisted addresses will block
--       - On response from zero cache:
--           - Zero untouched data and tags
--           - Remove address from blacklist
--           - Mark beat as non-zero in zero cache

