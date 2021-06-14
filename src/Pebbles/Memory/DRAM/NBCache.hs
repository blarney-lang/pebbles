-- Non-blocking set-associative write-back DRAM cache
--
-- +------------------------------+----------------------------------------------+
-- | Parameter                    | Description                                  |
-- +------------------------------+----------------------------------------------+
-- | NBDRAMCacheLogBeatsPerLine   | Cache line size                              |
-- | NBDRAMCacheLogNumWays        | Number of set-associative ways               |
-- | NBDRAMCacheLogSets           | Number of sets                               |
-- | NBDRAMCacheLogMaxInflight    | Max number of inflight memory requests       |
-- | NBDRAMCachePendingReqsPerWay | Max number of pending requests per way       |
-- +------------------------------+----------------------------------------------+

module Pebbles.Memory.DRAM.NBCache
  ( makeNBDRAMCache
  ) where

-- SoC parameters
#include <Config.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.Interconnect

-- Pebbles imports
import Pebbles.Util.Counter
import Pebbles.Util.SearchQueue
import Pebbles.Memory.DRAM.Interface

-- Types
-- =====

-- | DRAM request ids from the cache; contains the address in the
-- cache's data memery where the beat is to be stored
type NBDRAMCacheReqId = BeatIndex

-- | Index for a set in the tag memories
type SetIndex = Bit NBDRAMCacheLogSets

-- | Width of a beat index in data memory
type BeatIndexWidth = NBDRAMCacheLogNumWays +
       NBDRAMCacheLogSets + NBDRAMCacheLogBeatsPerLine

-- | Index for a beat in the data memory 
type BeatIndex = Bit BeatIndexWidth

-- | A tag holds the upper bits of an address
type Tag = Bit (DRAMAddrWidth -
                  (NBDRAMCacheLogSets +
                    NBDRAMCacheLogBeatsPerLine))

-- | Index of beat within line
type BeatId = Bit NBDRAMCacheLogBeatsPerLine

-- | Index of way within set
type WayId = Bit NBDRAMCacheLogNumWays

-- | Address of a cache line
type LineAddr = Bit (DRAMAddrWidth - NBDRAMCacheLogBeatsPerLine)

-- | Cache line state
data LineState =
  LineState {
    lineTag :: Tag
    -- ^ Tag (upper bits of address)
  , lineValid :: Bit 1
    -- ^ Valid bit
  , lineDirty :: Bit 1
    -- ^ Dirty bit (true if line has been modified)
  }
  deriving (Generic, Interface, Bits)

-- | Details of an inflight request being processed by the cache
data InflightInfo t_id =
  InflightInfo {
    inflightReq :: DRAMReq t_id
    -- ^ Original DRAM request
  , inflightWay :: WayId
    -- ^ Chosen set-associative way for this request
  , inflightHit :: Bit 1
    -- ^ Is it a cache hit?
  }
  deriving (Generic, Bits)

-- | Details of a miss being processed by the cache
data MissInfo =
  MissInfo {
    missSetId :: SetIndex
    -- ^ Set index of miss
  , missWay :: WayId
    -- ^ Chosen way for eviction
  , missLine :: LineState
    -- ^ State of line before eviction
  , missNewTag :: Tag
    -- ^ Upper address bits of new line being fetched
  }
  deriving (Generic, Bits)

-- Helper functions
-- ================

-- | Determine cache line address
lineAddr :: DRAMAddr -> LineAddr
lineAddr = slice @(DRAMAddrWidth-1) @NBDRAMCacheLogBeatsPerLine

-- | Determine the set index given the thread id and address
setIndex :: DRAMAddr -> SetIndex
setIndex addr = truncate (lineAddr addr)

-- | Determine the bits that make up a tag
getTag :: DRAMAddr -> Tag
getTag = upper
  
-- Implementation
-- ==============

-- | Non-blocking set-associate write-back DRAM cache.  Optimised for
-- throughput over latency.  Capable of a large number of inflight
-- requests (NBDRAMCacheLogMaxInflight). Capable of 100% throughput,
-- except when consuective requests access the same set (where a
-- pipeline bubble is inserted to avoid reading stale data); this
-- includes burst stores (a special case that could be optimised in
-- future).  Use fetch-on-write policy, which means that DRAM
-- bandwidth can be wasted in the case where the fetch is
-- unneccessary.
makeNBDRAMCache :: forall t_id. Bits t_id =>
     Stream (DRAMReq t_id)
     -- ^ Cache requests
  -> Stream (DRAMResp NBDRAMCacheReqId)
     -- ^ DRAM responses
  -> Module (Stream (DRAMResp t_id), Stream (DRAMReq NBDRAMCacheReqId))
     -- ^ Cache responses, DRAM requests
makeNBDRAMCache reqs dramResps = do
  -- Tag memories
  tagMems :: [RAM SetIndex LineState] <-
    replicateM (2^NBDRAMCacheLogNumWays) makeDualRAM

  -- Data memory
  (dataMemA :: RAMBE BeatIndexWidth DRAMBeatBytes,
     dataMemB :: RAMBE BeatIndexWidth DRAMBeatBytes) <-
       makeTrueDualRAMBE

  -- Lines can be reserved to ensure they are not evicted
  -- (They will be needed later and released)
  reservedQueues :: [SearchQueue SetIndex] <-
    replicateM (2^NBDRAMCacheLogNumWays)
               (makeSearchQueue NBDRAMCachePendingReqsPerWay)

  -- Inflight requests
  inflightQueue :: Queue (InflightInfo t_id) <-
    makeSizedQueue NBDRAMCacheLogMaxInflight

  -- Miss queue
  missQueue :: Queue MissInfo <-
    makeSizedQueue NBDRAMCacheLogMaxInflight

  -- DRAM requests
  dramReqQueue :: Queue (DRAMReq NBDRAMCacheReqId) <- makeQueue

  -- Cache responses
  respQueue :: Queue (DRAMResp t_id) <- makeQueue

  -- Tag lookup and update
  -- =====================
  --
  -- Two stage pipeline
  -- Stage 1: tag lookup
  -- Stage 2: tag update

  -- Way counter for eviction
  evictWay :: Reg WayId <- makeReg 0

  -- Beat counter for burst requests
  reqBeatCount :: Reg DRAMBurst <- makeReg 0

  -- Pipeline register for determining if line is reserved (one per way)
  busyRegs :: [Reg (Bit 1)] <-
    replicateM (2^NBDRAMCacheLogNumWays) (makeReg dontCare)

  -- Pipeline trigger (for second stage)
  go2 :: Reg (Bit 1) <- makeDReg false

  -- Request register (for second stage)
  reqReg :: Reg (DRAMReq t_id) <- makeReg dontCare

  -- Pipeline stall wire
  -- (Both first and second stages are stalled)
  stallWire :: Wire (Bit 1) <- makeWire false

  -- Insert pipeline bubble
  -- (Only first stage is stalled, and only for one cycle)
  -- (Used when consecutive requests access same state)
  bubbleWire :: Wire (Bit 1) <- makeWire false

  always do
    -- Load tags
    sequence [load tagMem (reqs.peek.dramReqAddr.setIndex) | tagMem <- tagMems]

    -- See if line is currently busy
    -- On a stall, we need to look at request from stage 2 rather than stage 1
    sequence
      [ busy <== member reservedQueue (stallWire.val ?
          (reqReg.val.dramReqAddr.setIndex, reqs.peek.dramReqAddr.setIndex))
      | (busy, reservedQueue) <- zip busyRegs reservedQueues ]

    -- Stage 1: tag lookup
    when (reqs.canPeek .&&. bubbleWire.val.inv .&&. stallWire.val.inv) do
      let req = reqs.peek
      -- Check some restrictions on burst requests
      let maxBeats = fromInteger (2^NBDRAMCacheLogBeatsPerLine)
      dynamicAssert (req.dramReqBurst .<=. maxBeats)
                    "NBDRAMCache: burst count exceeds beats-per-line"
      let beatOffset :: BeatId = req.dramReqAddr.truncate
      dynamicAssert (zeroExtend beatOffset + req.dramReqBurst .<=. maxBeats)
                    "NBDRAMCache: burst spans multiple lines"
      -- Trigger next stage
      go2 <== true
      reqReg <== req
      reqs.consume

    -- Stage 2: tag update
    when (go2.val) do
      let req = reqReg.val
      let setId = req.dramReqAddr.setIndex
      -- Look for a set-associative match
      let matches =
            [ let s = tagMem.out in
                s.lineValid .&&. s.lineTag .==. req.dramReqAddr.getTag
            | tagMem <- tagMems ]
      -- Cache hit?
      let isHit = orList matches
      let matchingWay = select $ zip matches (map fromInteger [0..])
      let chosenWay = isHit ? (matchingWay, evictWay.val)
      -- Stall condition
      let stall = -- It's a miss and line is busy
                  inv isHit .&&. (map val busyRegs ! evictWay.val)
             .||. -- The queue of inflight requests is full
                  inflightQueue.notFull.inv
             .||. -- The miss queue is full
                  missQueue.notFull.inv
             .||. -- The set of reserved lines is full
                  inv (map canInsert reservedQueues ! chosenWay)
      -- Evict a different way next time
      evictWay <== evictWay.val + 1
      -- Try to consume request
      if stall
        then do
          -- Stall pipeline
          go2 <== true
          stallWire <== true
          sequence_ [tagMem.preserveOut | tagMem <- tagMems]
        else do
          -- Count beats in burst store
          when (req.dramReqIsStore) do
            let reqBeatCountNew = reqBeatCount.val + 1
            if reqBeatCountNew .==. req.dramReqBurst
              then reqBeatCount <== 0
              else reqBeatCount <== reqBeatCountNew
          -- Update line state
          sequence
            [ when (way .==. chosenWay) do
                -- Don't update tag on load hit
                when (inv (isHit .&&. req.dramReqIsStore.inv)) do
                  store tagMem setId
                    LineState {
                      lineTag = req.dramReqAddr.getTag
                    , lineValid = true
                    , lineDirty = req.dramReqIsStore
                    }
            | (tagMem, way) <- zip tagMems (map fromInteger [0..]) ]
          -- Reserve line
          sequence
            [ when (way .==. chosenWay) do
                -- Only reserve line once for store burst
                when (req.dramReqIsStore .==>. reqBeatCount.val .==. 0) do
                  insert reservedQueue setId
            | (reservedQueue, way) <-
                zip reservedQueues (map fromInteger [0..]) ]
          -- Insert request into inflight queue
          enq inflightQueue 
            InflightInfo {
              inflightReq = req
            , inflightWay = chosenWay
            , inflightHit = isHit
            }
          -- Insert miss info into miss queue
          when (inv isHit) do
            enq missQueue
              MissInfo {
                missSetId = setId
              , missWay = evictWay.val
              , missLine = map out tagMems ! evictWay.val
              , missNewTag = req.dramReqAddr.getTag
              }
          -- Insert pipeline bubble when consecutive requests access same line
          when (reqs.canPeek .&&.
                  reqs.peek.dramReqAddr.setIndex .==.
                  req.dramReqAddr.setIndex) do
            bubbleWire <== true

  -- Miss handler
  -- ============

  -- Counter used to write back each beat of line
  writebackCount :: Reg (Bit (NBDRAMCacheLogBeatsPerLine+1)) <- makeReg 0

  -- Miss handler state machine
  -- State 0: writeback
  -- State 1: fetch
  missState :: Reg (Bit 1) <- makeReg 0

  -- Stall wire for writeback pipeline
  writebackStall :: Wire (Bit 1) <- makeWire 0

  -- For enabling writeback on subsequent cycle
  enableWriteback :: Reg (Bit 1) <- makeDReg false

  -- Resolve data memory conflict between miss handler and memory response
  -- (Priority given to memory response)
  dramRespInProgress :: Wire (Bit 1) <- makeWire false

  always do
    when (missQueue.canDeq) do
      let miss = missQueue.first
      -- No need to writeback on invalid or clean line
      if miss.missLine.lineValid.inv .||.
           miss.missLine.lineDirty.inv .||.
             missState.val .==. 1
        then do
          -- Fetch
          -- =====

          when (dramReqQueue.notFull) do
            -- Prepare fetch request
            let dramReq =
                  DRAMReq {
                    dramReqId =
                      miss.missWay # miss.missSetId # (0 :: BeatId)
                  , dramReqIsStore = false
                  , dramReqAddr =
                      miss.missNewTag # miss.missSetId # (0 :: BeatId)
                  , dramReqData = dontCare
                  , dramReqByteEn = 0
                  , dramReqBurst = fromInteger (2^NBDRAMCacheLogBeatsPerLine)
                  , dramReqIsFinal = true
                  }
            -- Issue fetch
            enq dramReqQueue dramReq
            -- Consume miss
            missQueue.deq
            -- Reset for next writeback
            writebackCount <== 0
            -- Next state
            missState <== 0
        else do
          -- Writeback
          -- =========

          -- Give way to memory response stage
          -- (Which also accesses data memory)
          when (dramRespInProgress.val.inv) do
            -- Load beat from data memory
            loadBE dataMemA
              (miss.missWay # miss.missSetId # writebackCount.val.truncate)
            -- Try writeback on next cycle
            enableWriteback <== true
            -- Increment writeback count
            when (writebackStall.val.inv) do
              writebackCount <== writebackCount.val + 1
          -- Try writeback
          when (enableWriteback.val) do
            let writebackFinished = writebackCount.val .==.
                 fromInteger (2^NBDRAMCacheLogBeatsPerLine)
            -- Prepare writeback request
            let dramReq =
                  DRAMReq {
                    dramReqId = dontCare
                  , dramReqIsStore = true
                  , dramReqAddr =
                      miss.missLine.lineTag # miss.missSetId # (0 :: BeatId)
                  , dramReqData = dataMemA.outBE
                  , dramReqByteEn = ones
                  , dramReqBurst = fromInteger (2^NBDRAMCacheLogBeatsPerLine)
                  , dramReqIsFinal = writebackFinished
                  }
            -- Try to submit DRAM request
            if dramReqQueue.notFull
              then do
                enq dramReqQueue dramReq
                -- Move to fetch state
                when writebackFinished do
                  missState <== 1
              else do
                -- Retry on next cycle
                writebackStall <== true
                enableWriteback <== true
                dataMemA.preserveOutBE

  -- Memory response
  -- ===============

  -- Count number of completed fetches
  -- Used to determine when inflight requests can be processed
  fetchDoneCount :: Counter (NBDRAMCacheLogMaxInflight+1) <-
    makeCounter (fromInteger (2^NBDRAMCacheLogMaxInflight))

  -- Count beats in DRAM response burst
  dramRespBeatCount :: Reg (Bit NBDRAMCacheLogBeatsPerLine) <- makeReg 0

  always do
    when (dramResps.canPeek) do
      let resp = dramResps.peek
      -- Take control over data mem port A
      dramRespInProgress <== true
      -- Write beat to data memory
      storeBE dataMemA (resp.dramRespId + dramRespBeatCount.val.zeroExtend)
        ones (dramResps.peek.dramRespData)
      -- Consume request
      dramResps.consume
      dramRespBeatCount <== dramRespBeatCount.val + 1
      -- At end of burst, increment completed fetches
      when (dramRespBeatCount.val .==. ones) do
        incrBy fetchDoneCount 1
      -- Completed fetches can never exceed number of inflight requests
      dynamicAssert (fetchDoneCount.isFull.inv)
        "NBDRAMCache: broken invariant on fetchDoneCount"

  -- Cache response
  -- ==============

  -- Pulsed when response queue is being consumed
  respConsumeWire :: Wire (Bit 1) <- makeWire false

  -- For bursts
  respBeatCount :: Reg (Bit NBDRAMCacheLogBeatsPerLine) <- makeReg 0

  -- Track current size of respQueue
  respCount :: Counter 2 <- makeCounter 2

  -- Trigger for second pipeline stage
  issueResp :: Reg (Bit 1) <- makeDReg false

  -- Request id for second pipeline stage
  respIdReg :: Reg t_id <- makeReg dontCare

  -- Stage 1: access data memory
  always do
    when (respQueue.notFull .&&. inflightQueue.canDeq) do
      let inflight = inflightQueue.first
      let req = inflight.inflightReq
      -- Stall condition
      let stall = -- Miss has not yet been resolved
                  inflight.inflightHit.inv .&&. fetchDoneCount.getCount .==. 0
             .||. -- Response buffer is full
                  respCount.isFull .&&. respConsumeWire.val.inv
      -- Process request
      when (inv stall) do
        -- Determine address for access to cache's data memory
        let dataMemAddr = (inflight.inflightWay # req.dramReqAddr.truncate) +
              respBeatCount.val.zeroExtend
        -- Lookup / update data memory
        if req.dramReqIsStore
          then do
            storeBE dataMemB
              dataMemAddr
              (req.dramReqByteEn)
              (req.dramReqData)
            -- Consume store request
            inflightQueue.deq
            -- Decrement fetch count on store miss
            -- (Only one store in a burst will be marked as a miss)
            when (inflight.inflightHit.inv) do
              decrBy fetchDoneCount 1
          else do
            loadBE dataMemB dataMemAddr
            -- Increment response count
            incrBy respCount 1
            -- Trigger next pipeline stage
            issueResp <== true
            respIdReg <== req.dramReqId
        -- End of burst?
        if respBeatCount.val .==. truncate (req.dramReqBurst-1)
          then do
            -- Reset count for next time
            respBeatCount <== 0
            -- Consume burst load request
            when (req.dramReqIsStore.inv) do
              inflightQueue.deq
              -- Decrement fetch count on load miss
              when (inflight.inflightHit.inv) do
                decrBy fetchDoneCount 1
            -- Release line
            sequence_
              [ when (way .==. inflight.inflightWay) do
                  delete reservedQueue
              | (reservedQueue, way) <-
                  zip reservedQueues (map fromInteger [0..]) ]
          else do
            respBeatCount <== respBeatCount.val + 1

  -- Stage 2: issue response
  always do
    when (issueResp.val) do
      -- Ensure response queue has space
      dynamicAssert (respQueue.notFull) "NBDRAMCache: response queue overflow"
      -- Enqueue response
      enq respQueue
        DRAMResp {
          dramRespId = respIdReg.val
        , dramRespBurstId = respBeatCount.val.old.zeroExtend
        , dramRespData = dataMemB.outBE
        }

  return
    ( Source {
        peek = respQueue.first
      , canPeek = respQueue.canDeq
      , consume = do
          respQueue.deq
          decrBy respCount 1
          respConsumeWire <== true
      }
    , dramReqQueue.toStream
    )
