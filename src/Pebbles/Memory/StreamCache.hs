-- Non-blocking set-associative write-back DRAM cache
--
-- +------------------------------+-------------------------------------------+
-- | Parameter                    | Description                               |
-- +------------------------------+-------------------------------------------+
-- | StreamCacheLogItemsPerBeat   | Number of data items per DRAM beat        |
-- | StreamCacheLogBeatsPerLine   | Cache line size                           |
-- | StreamCacheLogNumWays        | Number of set-associative ways            |
-- | StreamCacheLogSets           | Number of sets                            |
-- | StreamCacheLogMaxInflight    | Max number of inflight memory requests    |
-- | StreamCachePendingReqsPerWay | Max number of pending requests per way    |
-- +------------------------------+-------------------------------------------+

module Pebbles.Memory.StreamCache
  ( makeStreamCache
  , StreamCacheReq(..)
  , StreamCacheResp(..)
  ) where

-- SoC parameters
#include <Config.h>

-- TODO: temporary
#define StreamCacheLogItemsPerBeat   0
#define StreamCacheLogBeatsPerLine   1
#define StreamCacheLogNumWays        2
#define StreamCacheLogSets           7
#define StreamCacheLogMaxInflight    5
#define StreamCachePendingReqsPerWay 8

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.Interconnect
import qualified Blarney.Vector as V

-- Pebbles imports
import Pebbles.Util.Counter
import Pebbles.Util.SearchQueue
import Pebbles.Memory.DRAM.Interface

-- Types
-- =====

-- | Data width of items being accessed
type StreamCacheDataWidth =
  Div DRAMBeatBits (2^StreamCacheLogItemsPerBeat)

-- | Data item
type StreamCacheData = Bit StreamCacheDataWidth

-- | Stream cache request
data StreamCacheReq t_id =
  StreamCacheReq {
    streamCacheReqId :: t_id
    -- ^ Request id
  , streamCacheReqIsStore :: Bit 1
    -- ^ Is it a load or store?
  , streamCacheReqAddr :: DRAMAddr
    -- ^ Beat address
  , streamCacheReqOffset :: Bit StreamCacheLogItemsPerBeat
    -- ^ Offset of item within beat
  , streamCacheReqData :: StreamCacheData
    -- ^ Data to write
  , streamCacheReqWriteEn :: StreamCacheData
    -- ^ Write-enable bit mask
  }
  deriving (Generic, Interface, Bits)

-- Stream cache response
data StreamCacheResp t_id =
  StreamCacheResp {
    streamCacheRespId :: t_id
    -- ^ Response id
  , streamCacheRespData :: StreamCacheData
    -- ^ Response data
  }
  deriving (Generic, Interface, Bits)

-- | DRAM request ids from the cache; contains the address in the
-- cache's data memory where the beat is to be stored
type StreamCacheReqId = BeatIndex

-- | Index for a set in the cache's tag memories
type SetIndex = Bit StreamCacheLogSets

-- | Width of a beat index in the cache's data memory
type BeatIndexWidth = StreamCacheLogNumWays +
       StreamCacheLogSets + StreamCacheLogBeatsPerLine

-- | Index for a beat in the data memory 
type BeatIndex = Bit BeatIndexWidth

-- | A tag holds the upper bits of a beat address
type Tag = Bit (DRAMAddrWidth -
                  (StreamCacheLogSets +
                    StreamCacheLogBeatsPerLine))

-- | Index of beat within line
type BeatId = Bit StreamCacheLogBeatsPerLine

-- | Index of way within set
type WayId = Bit StreamCacheLogNumWays

-- | Address of a cache line
type LineAddr = Bit (DRAMAddrWidth - StreamCacheLogBeatsPerLine)

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
    inflightReq :: StreamCacheReq t_id
    -- ^ Original request
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
lineAddr = slice @(DRAMAddrWidth-1) @StreamCacheLogBeatsPerLine

-- | Determine the set index given the thread id and address
setIndex :: DRAMAddr -> SetIndex
setIndex addr = truncate (lineAddr addr)

-- | Determine the bits that make up a tag
getTag :: DRAMAddr -> Tag
getTag = upper

-- Implementation
-- ==============

-- | Non-blocking set-associative write-back cache.  Optimised
-- for throughput over latency.  Capable of a large number of inflight
-- requests (StreamCacheLogMaxInflight). Capable of 100% throughput,
-- except when consuective requests access the same set (where a
-- pipeline bubble is inserted to avoid reading stale data); this
-- includes burst stores (a special case that could be optimised in
-- future).  Uses write-allocate policy, which means that DRAM
-- bandwidth can be wasted in the case where the fetch is
-- unneccessary.
makeStreamCache :: forall t_id. Bits t_id =>
     Stream (StreamCacheReq t_id)
     -- ^ Cache requests
  -> Stream (DRAMResp StreamCacheReqId)
     -- ^ DRAM responses
  -> Module (Stream (StreamCacheResp t_id), Stream (DRAMReq StreamCacheReqId))
     -- ^ Cache responses, DRAM requests
makeStreamCache reqs dramResps = do
  -- Tag memories
  tagMems :: [RAM SetIndex LineState] <-
    replicateM (2^StreamCacheLogNumWays) makeDualRAM

  -- Data memory
  (dataMemA :: RAMBE BeatIndexWidth DRAMBeatBytes,
     dataMemB :: RAMBE BeatIndexWidth DRAMBeatBytes) <-
       makeTrueDualRAMBE

  -- Lines can be reserved to ensure they are not evicted
  -- (until they are used)
  reservedQueues :: [SearchQueue SetIndex] <-
    replicateM (2^StreamCacheLogNumWays)
               (makeSearchQueue StreamCachePendingReqsPerWay)

  -- Inflight requests
  inflightQueue :: Queue (InflightInfo t_id) <-
    makeSizedQueue StreamCacheLogMaxInflight

  -- Miss queue
  missQueue :: Queue MissInfo <-
    makeSizedQueue StreamCacheLogMaxInflight

  -- DRAM requests
  dramReqQueue :: Queue (DRAMReq StreamCacheReqId) <- makeQueue

  -- Cache responses
  respQueue :: Queue (StreamCacheResp t_id) <- makeQueue

  -- Tag lookup and update
  -- =====================
  --
  -- Two stage pipeline
  -- Stage 1: tag lookup
  -- Stage 2: tag update

  -- Way counter for eviction
  evictWay :: Reg WayId <- makeReg 0

  -- Pipeline register for determining if line is reserved (one per way)
  busyRegs :: [Reg (Bit 1)] <-
    replicateM (2^StreamCacheLogNumWays) (makeReg dontCare)

  -- Pipeline trigger (for second stage)
  go2 :: Reg (Bit 1) <- makeDReg false

  -- Request register (for second stage)
  reqReg :: Reg (StreamCacheReq t_id) <- makeReg dontCare

  -- Pipeline stall wire
  -- (Both first and second stages are stalled)
  stallWire :: Wire (Bit 1) <- makeWire false

  -- Insert pipeline bubble
  -- (Only first stage is stalled, and only for one cycle)
  -- (Used when consecutive requests access same state)
  bubbleWire :: Wire (Bit 1) <- makeWire false

  always do
    -- Load tags
    sequence
      [ load tagMem (reqs.peek.streamCacheReqAddr.setIndex)
      | tagMem <- tagMems ]

    -- See if line is currently busy
    -- On a stall, we need to look at request from stage 2 rather than stage 1
    sequence
      [ busy <== member reservedQueue (stallWire.val ?
          (reqReg.val.streamCacheReqAddr.setIndex,
           reqs.peek.streamCacheReqAddr.setIndex))
      | (busy, reservedQueue) <- zip busyRegs reservedQueues ]

    -- Stage 1: tag lookup
    when (reqs.canPeek .&&. bubbleWire.val.inv .&&. stallWire.val.inv) do
      let req = reqs.peek
      -- Trigger next stage
      go2 <== true
      reqReg <== req
      reqs.consume

    -- Stage 2: tag update
    when (go2.val) do
      let req = reqReg.val
      let setId = req.streamCacheReqAddr.setIndex
      -- Look for a set-associative match
      let matches =
            [ let s = tagMem.out in
                s.lineValid .&&. s.lineTag .==. req.streamCacheReqAddr.getTag
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
          -- Update line state
          sequence
            [ when (way .==. chosenWay) do
                -- Don't update tag on load hit
                when (inv (isHit .&&. req.streamCacheReqIsStore.inv)) do
                  store tagMem setId
                    LineState {
                      lineTag = req.streamCacheReqAddr.getTag
                    , lineValid = true
                    , lineDirty = req.streamCacheReqIsStore
                    }
            | (tagMem, way) <- zip tagMems (map fromInteger [0..]) ]
          -- Reserve line
          sequence
            [ when (way .==. chosenWay) do
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
              , missNewTag = req.streamCacheReqAddr.getTag
              }
          -- Insert pipeline bubble when consecutive requests access same line
          when (reqs.canPeek .&&.
                  reqs.peek.streamCacheReqAddr.setIndex .==.
                  req.streamCacheReqAddr.setIndex) do
            bubbleWire <== true

  -- Miss handler
  -- ============

  -- Counter used to write back each beat of line
  writebackCount :: Reg (Bit (StreamCacheLogBeatsPerLine+1)) <- makeReg 0

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
                  , dramReqDataTagBits = dontCare
                  , dramReqByteEn = 0
                  , dramReqBurst = fromInteger (2^StreamCacheLogBeatsPerLine)
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
                 fromInteger (2^StreamCacheLogBeatsPerLine)
            -- Prepare writeback request
            let dramReq =
                  DRAMReq {
                    dramReqId = dontCare
                  , dramReqIsStore = true
                  , dramReqAddr =
                      miss.missLine.lineTag # miss.missSetId # (0 :: BeatId)
                  , dramReqData = dataMemA.outBE
                  , dramReqDataTagBits = dontCare
                  , dramReqByteEn = ones
                  , dramReqBurst = fromInteger (2^StreamCacheLogBeatsPerLine)
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
  fetchDoneCount :: Counter (StreamCacheLogMaxInflight+1) <-
    makeCounter (fromInteger (2^StreamCacheLogMaxInflight))

  -- Count beats in DRAM response burst
  dramRespBeatCount :: Reg (Bit StreamCacheLogBeatsPerLine) <- makeReg 0

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
        "StreamCache: broken invariant on fetchDoneCount"

  -- Cache response
  -- ==============

  -- Pulsed when response queue is being consumed
  respConsumeWire :: Wire (Bit 1) <- makeWire false

  -- Track current size of respQueue
  respCount :: Counter 2 <- makeCounter 2

  -- Trigger for second pipeline stage
  issueResp :: Reg (Bit 1) <- makeDReg false

  -- State machine
  -- State 0: data lookup
  -- State 1: data update
  respState :: Reg (Bit 1) <- makeReg 0

  -- Request id for second pipeline stage
  respIdReg :: Reg t_id <- makeReg dontCare

  -- Beat offset for second pipeline stage
  respBeatOffset :: Reg (Bit StreamCacheLogItemsPerBeat) <- makeReg dontCare

  -- Stage 1: access data memory
  always do
    let inflight = inflightQueue.first
    let req = inflight.inflightReq
    -- Determine address for access to cache's data memory
    let dataMemAddr = inflight.inflightWay #
                        req.streamCacheReqAddr.truncate
    -- State 0: data lookup
    when (respState.val .==. 0) do
      when (respQueue.notFull .&&. inflightQueue.canDeq) do
        -- Stall condition
        let stall = -- Miss has not yet been resolved
                    inflight.inflightHit.inv .&&.
                      fetchDoneCount.getCount .==. 0
               .||. -- Response buffer is full
                    respCount.isFull .&&. respConsumeWire.val.inv
        -- Process request
        when (inv stall) do
          -- Lookup data memory
          loadBE dataMemB dataMemAddr
          -- Handle loads and stores
          if req.streamCacheReqIsStore
            then do
              -- Move to update state
              respState <== 1
            else do
              -- Increment response count
              incrBy respCount 1
              -- Consume request
              inflightQueue.deq
              -- Trigger next pipeline stage
              issueResp <== true
              respIdReg <== req.streamCacheReqId
              respBeatOffset <== req.streamCacheReqOffset
          -- Decrement fetch count on miss
          when (inflight.inflightHit.inv) do
            decrBy fetchDoneCount 1
          -- Release line
          sequence_
            [ when (way .==. inflight.inflightWay) do
                delete reservedQueue
            | (reservedQueue, way) <-
                zip reservedQueues (map fromInteger [0..]) ]

    -- State 1: data update
    when (respState.val .==. 1) do
      dynamicAssert (inflightQueue.canDeq)
        "StreamCache: inflight queue unexpectedly empty"
      -- Consume store request
      inflightQueue.deq
      -- Mask capturing which bits we're writing to
      let mask :: V.Vec (2^StreamCacheLogItemsPerBeat) StreamCacheData =
            V.map (\i -> 
              if req.streamCacheReqOffset .==. fromInteger i
                then req.streamCacheReqWriteEn else 0)
                  V.genVec
      -- New data (before mask applied)
      let newData :: V.Vec (2^StreamCacheLogItemsPerBeat) StreamCacheData =
            V.replicate (req.streamCacheReqData)
      -- Data to write  (after mask applied)
      let writeData :: DRAMBeat =
            fromBitList $
              [ cond ? (new, old)
              | (cond, old, new) <-
                  zip3 (toBitList $ pack mask)
                       (toBitList $ dataMemB.outBE)
                       (toBitList $ pack newData) ]
      -- Update data memory
      storeBE dataMemB
        dataMemAddr
        ones
        writeData
      -- Move back to initial state
      respState <== 0

  -- Stage 2: issue response
  always do
    when (issueResp.val) do
      -- Ensure response queue has space
      dynamicAssert (respQueue.notFull) "StreamCache: response queue overflow"
      -- Split response item items
      let dataItems :: V.Vec (2^StreamCacheLogItemsPerBeat) StreamCacheData =
            dataMemB.outBE.unpack
      -- Enqueue response
      enq respQueue
        StreamCacheResp {
          streamCacheRespId = respIdReg.val
        , streamCacheRespData =
            dataItems ! respBeatOffset.val
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
