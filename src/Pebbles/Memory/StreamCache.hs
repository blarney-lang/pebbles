-- Non-blocking set-associative write-back DRAM cache
--
-- +------------------------------+-----------------------------------------+
-- | Parameter                    | Description                             |
-- +------------------------------+-----------------------------------------+
-- | StreamCacheLogItemsPerBeat   | Number of data items per DRAM beat      |
-- | StreamCacheLogBeatsPerLine   | Cache line size                         |
-- | StreamCacheLogNumWays        | Number of set-associative ways          |
-- | StreamCacheLogSets           | Number of sets                          |
-- | StreamCacheLogMaxInflight    | Max number of inflight memory requests  |
-- | StreamCachePendingReqsPerWay | Max number of pending requests per way  |
-- | StreamCacheAddrWidth         | Width of beat address in bits           |
-- +------------------------------+-----------------------------------------+

-- SoC parameters
#include <Config.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.Option
import Blarney.Interconnect
import qualified Blarney.Vector as V

-- Pebbles imports
import Pebbles.Util.Counter
import Pebbles.Util.SearchQueue
import Pebbles.Memory.DRAM.Interface

-- Haskell imports
import Data.List (zip4)

-- Types
-- =====

-- | Data width of items being accessed
type StreamCacheDataWidth =
  Div DRAMBeatBits (2^StreamCacheLogItemsPerBeat)

-- | Data item
type StreamCacheData = Bit StreamCacheDataWidth

-- | Address
type StreamCacheAddr = Bit StreamCacheAddrWidth

-- | Stream cache request
data StreamCacheReq t_id =
  StreamCacheReq {
    streamCacheReqId :: t_id
    -- ^ Request id
  , streamCacheReqIsStore :: Bit 1
    -- ^ Is it a load or store?
  , streamCacheReqAddr :: StreamCacheAddr
    -- ^ Beat address
  , streamCacheReqOffset :: Bit StreamCacheLogItemsPerBeat
    -- ^ Offset of item within beat
  , streamCacheReqData :: StreamCacheData
    -- ^ Data to write
  , streamCacheReqWriteEn :: StreamCacheData
    -- ^ Write-enable bit mask
  , streamCacheReqZeroBits :: Option DRAMBeat
    -- ^ When enabled, this field allows all the bits of the beat
    -- being accessed to be set to zero, according to the given mask
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

-- | DRAM request ids from the cache
data StreamCacheReqId =
  StreamCacheReqId {
    beatIndex     :: BeatIndex
    -- ^ Contains the address in the  cache's data memory
    -- where the beat is to be stored
  , beatKnownZero :: Bit 1
    -- ^ Is data being fetched known to be zero
  }
  deriving (Generic, Interface, Bits)

-- | Index for a set in the cache's tag memories
type SetIndex = Bit StreamCacheLogSets

-- | Width of a beat index in the cache's data memory
type BeatIndexWidth = StreamCacheLogNumWays +
       StreamCacheLogSets + StreamCacheLogBeatsPerLine

-- | Index for a beat in the data memory 
type BeatIndex = Bit BeatIndexWidth

-- | A tag holds the upper bits of a beat address
type Tag = Bit (StreamCacheAddrWidth -
                  (StreamCacheLogSets +
                    StreamCacheLogBeatsPerLine))

-- | Index of beat within line
type BeatId = Bit StreamCacheLogBeatsPerLine

-- | Index of way within set
type WayId = Bit StreamCacheLogNumWays

-- | Address of a cache line
type LineAddr = Bit (StreamCacheAddrWidth - StreamCacheLogBeatsPerLine)

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
  , inflightReplace :: Bit 1
    -- ^ Does a line need to be replaced?
  , inflightZero :: Bit 1
    -- ^ Is cache line being accessed known to be zero?
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
  , missZero :: Bit 1
    -- ^ Is cache line being accessed known to be zero?
  }
  deriving (Generic, Bits)

-- | Stream cache config options
data StreamCacheConfig =
  StreamCacheConfig {
    useZeroTable :: Bool
    -- ^ Track cache lines known to be zero.  Initialises memory to
    -- zero and improves hit rate when memory fetched is often zero.
  , baseAddr :: Bit DRAMAddrWidth
    -- ^ Base address of region being cached
  }

-- Helper functions
-- ================

-- | Determine cache line address
lineAddr :: StreamCacheAddr -> LineAddr
lineAddr = slice @(StreamCacheAddrWidth-1) @StreamCacheLogBeatsPerLine

-- | Determine the set index given the thread id and address
setIndex :: StreamCacheAddr -> SetIndex
setIndex addr = truncate (lineAddr addr)

-- | Determine the bits that make up a tag
getTag :: StreamCacheAddr -> Tag
getTag = upper

-- Implementation
-- ==============

-- | Non-blocking set-associative write-back cache.  Optimised for
-- throughput over latency.  Capable of a large number of inflight
-- requests (StreamCacheLogMaxInflight).  Supports an optional
-- initialise-to-zero feature for backing memory, along with an
-- optimisation to track cache lines known to be zero without
-- accessing backing memory.
makeStreamCache :: forall t_id. Bits t_id =>
     StreamCacheConfig
     -- ^ Config options
  -> Stream (StreamCacheReq t_id)
     -- ^ Cache requests
  -> Stream (DRAMResp StreamCacheReqId)
     -- ^ DRAM responses
  -> Module (Stream (StreamCacheResp t_id), Stream (DRAMReq StreamCacheReqId))
     -- ^ Cache responses, DRAM requests
makeStreamCache config reqs dramResps = do
  -- Tag memories
  tagMems :: [RAM SetIndex LineState] <-
    replicateM (2^StreamCacheLogNumWays) makeDualRAMForward

  -- Data memory
  (dataMemA :: RAMBE BeatIndexWidth DRAMBeatBytes,
     dataMemB :: RAMBE BeatIndexWidth DRAMBeatBytes) <-
       makeTrueDualRAMBE

  -- Lines can be reserved to ensure they are not evicted
  -- (until they are used)
  reservedQueues :: [SearchQueue SetIndex] <-
    replicateM (2^StreamCacheLogNumWays)
               (makeSearchQueue True StreamCachePendingReqsPerWay)

  -- Inflight requests
  inflightQueue :: Queue (InflightInfo t_id) <-
    makeSizedQueueCore StreamCacheLogMaxInflight

  -- Miss queue
  missQueue :: Queue MissInfo <-
    makeSizedQueueCore StreamCacheLogMaxInflight

  -- DRAM requests
  dramReqQueue :: Queue (DRAMReq StreamCacheReqId) <- makeQueue

  -- Cache responses
  respQueue :: Queue (StreamCacheResp t_id) <-
    makeSizedQueue StreamCacheLogMaxInflight

  -- Which cache lines are known zero? (Active low)
  zeroTable :: RAM LineAddr (Bit 1) <-
    if config.useZeroTable
      then makeDualRAMForward
      else return (nullRAM { out = true })

  -- Tag lookup and update
  -- =====================
  --
  -- Three stage pipeline
  -- Stage 1: tag lookup
  -- Stage 2: latch
  -- Stage 3: tag update

  -- Pipeline triggers
  go2 :: Reg (Bit 1) <- makeDReg false
  go3 :: Reg (Bit 1) <- makeDReg false

  -- Request register (per stage)
  reqReg2 :: Reg (StreamCacheReq t_id) <- makeReg dontCare
  reqReg3 :: Reg (StreamCacheReq t_id) <- makeReg dontCare

  -- Latch register for zero table lookup
  isZero3 :: Reg (Bit 1) <- makeReg dontCare

  -- Latch registers for tag mem lookup
  tagRegs3 :: [Reg LineState] <-
    replicateM (2^StreamCacheLogNumWays) (makeReg dontCare)

  -- Tag write wire, per tag mem, for pipeline forwarding
  tagWrites :: [Wire LineState] <-
    replicateM (2^StreamCacheLogNumWays) (makeWire dontCare)

  -- Zero table write wire, for pipeline forwarding
  zeroWrite :: Wire (Bit 1) <- makeWire false

  -- Pipeline stall wire
  stallWire :: Wire (Bit 1) <- makeWire false

  -- Way counter for eviction
  evictWay :: Reg WayId <- makeReg 0

  -- Pipeline register for determining if line is reserved (one per way)
  busyRegs :: [Reg (Bit 1)] <-
    replicateM (2^StreamCacheLogNumWays) (makeReg dontCare)

  -- These signals become active if writeback detects a zeroed cache line
  let useZeroTableBit = if config.useZeroTable then true else false
  lineZeroedQueue :: Queue LineAddr <- makeQueue
  lineZeroed2 :: Reg (Bit 1) <- makeReg dontCare
  lineZeroed3 :: Reg (Bit 1) <- makeReg dontCare

  always do
    -- On stall, replay active stages and preserve RAM outputs
    when stallWire.val do
      go2 <== go2.val
      go3 <== go3.val
      sequence_ [tagMem.preserveOut | tagMem <- tagMems]
      zeroTable.preserveOut

    -- Load tags
    sequence
      [ load tagMem (setIndex reqs.peek.streamCacheReqAddr)
      | tagMem <- tagMems ]

    -- Lookup zero table
    load zeroTable (upper reqs.peek.streamCacheReqAddr)

    -- See if line is currently busy
    sequence
      [ busyReg <== reservedQueue.member
                      (stallWire.val ?
                        ( setIndex reqReg3.val.streamCacheReqAddr
                        , setIndex reqReg2.val.streamCacheReqAddr ))
      | (busyReg, reservedQueue) <- zip busyRegs reservedQueues ]

    -- Stage 1: tag lookup
    when ((reqs.canPeek .||. lineZeroedQueue.canDeq)
             .&&. inv stallWire.val) do
      if lineZeroedQueue.canDeq .&&. useZeroTableBit
        then do
          lineZeroedQueue.deq
          reqReg2 <== dontCare
                        { streamCacheReqAddr = lineZeroedQueue.first # 0 }
          lineZeroed2 <== true
        else do
          reqs.consume
          reqReg2 <== reqs.peek
          lineZeroed2 <== false
      -- Trigger next stage
      go2 <== true

    -- Stage 2: latch
    when (go2.val .&&. inv stallWire.val) do
      -- Latch RAM outputs, with pipeline forwarding
      sequence
        [ tagReg <== 
            (tagWrite.active .&&.
              setIndex reqReg2.val.streamCacheReqAddr .==.
              setIndex reqReg3.val.streamCacheReqAddr) ?
                (tagWrite.val, tagMem.out)
        | (tagReg, tagWrite, tagMem) <- zip3 tagRegs3 tagWrites tagMems ]
      isZero3 <==
        if config.useZeroTable
          then (zeroWrite.active .&&.
                  lineAddr reqReg2.val.streamCacheReqAddr .==.
                  lineAddr reqReg3.val.streamCacheReqAddr) ?
                    (zeroWrite.val, inv zeroTable.out)
          else false
      -- Trigger next stage
      go3 <== true
      reqReg3 <== reqReg2.val
      lineZeroed3 <== lineZeroed2.val

    -- Stage 3: tag update
    when go3.val do
      let req = reqReg3.val
      let setId = setIndex req.streamCacheReqAddr
      -- Look for a set-associative match
      let matches =
            [ let s = tagReg.val in
                s.lineValid .&&. s.lineTag .==. getTag req.streamCacheReqAddr
            | tagReg <- tagRegs3 ]
      -- Cache hit?
      let isHit = orList matches
      let matchingWay = select $ zip matches (map fromInteger [0..])
      let chosenWay = isHit ? (matchingWay, evictWay.val)
      let replace = inv isHit .&&.
            (if isZero3.val then req.streamCacheReqIsStore else true)
      -- Stall condition
      let stall = -- We need to replace and line is busy
                  replace .&&. (map (.val) busyRegs ! evictWay.val)
             .||. -- The queue of inflight requests is full
                  inv inflightQueue.notFull
             .||. -- The miss queue is full
                  inv missQueue.notFull
             .||. -- The set of reserved lines is full
                  inv (map canInsert reservedQueues ! chosenWay)
      -- Evict a different way next time
      evictWay <== evictWay.val + 1
      -- Are we marking a line as zero or processing a request?
      if lineZeroed3.val .&&. useZeroTableBit
        then do
          zeroWrite <== true
          zeroTable.store (lineAddr req.streamCacheReqAddr) false
        else do
          -- Try to consume request
          if stall
            then stallWire <== true
            else do
              -- Ignores stores of zero when line already known to be zero
              when (inv (isZero3.val .&&.
                           req.streamCacheReqIsStore .&&.
                             req.streamCacheReqData .==. 0)) do
                -- Update line state
                sequence
                  [ when (way .==. chosenWay) do
                      -- Update tag memory?
                      let doUpdate = replace
                                .||. req.streamCacheReqIsStore .&&.
                                       inv tagReg.val.lineDirty
                      when doUpdate do
                        let newTag =
                              LineState {
                                lineTag = getTag req.streamCacheReqAddr
                              , lineValid = true
                              , lineDirty = req.streamCacheReqIsStore
                              }
                        tagWrite <== newTag
                        tagMem.store setId newTag
                  | (tagMem, tagReg, tagWrite, way) <-
                      zip4 tagMems tagRegs3 tagWrites (map fromInteger [0..]) ]
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
                  , inflightReplace = replace
                  , inflightZero = isZero3.val
                  }
                -- Insert miss info into miss queue
                when replace do
                  enq missQueue
                    MissInfo {
                      missSetId = setId
                    , missWay = evictWay.val
                    , missLine = map (.val) tagRegs3 ! evictWay.val
                    , missNewTag = getTag req.streamCacheReqAddr
                    , missZero = isZero3.val
                    }
                -- Update zero table
                when (isZero3.val .&&. req.streamCacheReqIsStore) do
                  zeroWrite <== false
                  zeroTable.store (lineAddr req.streamCacheReqAddr) true

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

  -- Detect if we are writing back a zeroed cache line
  writebackZero :: Reg (Bit 1) <- makeReg true

  always do
    when (missQueue.canDeq) do
      let miss = missQueue.first
      -- No need to writeback on invalid or clean line
      if inv miss.missLine.lineValid .||.
           inv miss.missLine.lineDirty .||.
             missState.val .==. 1
        then do
          -- Fetch
          -- =====

          when (dramReqQueue.notFull) do
            -- Prepare fetch request
            let dramReq =
                  DRAMReq {
                    dramReqId =
                      StreamCacheReqId {
                        beatIndex =
                          miss.missWay # miss.missSetId # (0 :: BeatId)
                      , beatKnownZero = miss.missZero
                      }
                  , dramReqIsStore = false
                  , dramReqIsFastZero = false
                  , dramReqAddr = config.baseAddr + zeroExtend
                      (miss.missNewTag # miss.missSetId # (0 :: BeatId))
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
          when (inv dramRespInProgress.val .&&. inv writebackStall.val) do
            -- Load beat from data memory
            loadBE dataMemA
              (miss.missWay # miss.missSetId # truncate writebackCount.val)
            -- Try writeback on next cycle
            enableWriteback <== true
            -- Increment writeback count
            writebackCount <== writebackCount.val + 1
          -- Try writeback
          when (enableWriteback.val) do
            let writebackFinished = writebackCount.val .==.
                 fromInteger (2^StreamCacheLogBeatsPerLine)
            -- Prepare writeback request
            let addr = zeroExtend
                  (miss.missLine.lineTag # miss.missSetId # (0 :: BeatId))
            let dramReq =
                  DRAMReq {
                    dramReqId = dontCare
                  , dramReqIsStore = true
                  , dramReqIsFastZero = false
                  , dramReqAddr = config.baseAddr + addr
                  , dramReqData = dataMemA.outBE
                  , dramReqDataTagBits = dontCare
                  , dramReqByteEn = ones
                  , dramReqBurst = fromInteger (2^StreamCacheLogBeatsPerLine)
                  , dramReqIsFinal = writebackFinished
                  }
            let newWritebackZero = writebackZero.val .&&. dataMemA.outBE .==. 0
            -- Try to submit DRAM request
            if dramReqQueue.notFull .&&. lineZeroedQueue.notFull
              then do
                enq dramReqQueue dramReq
                -- Move to fetch state
                if writebackFinished
                  then do
                    missState <== 1
                    writebackZero <== true
                    when (useZeroTableBit .&&. newWritebackZero) do
                      lineZeroedQueue.enq (upper addr)
                  else do
                    writebackZero <== newWritebackZero
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
      storeBE dataMemA (resp.dramRespId.beatIndex +
                          zeroExtend dramRespBeatCount.val) ones
        (if resp.dramRespId.beatKnownZero
           then zero else dramResps.peek.dramRespData)
      -- Consume response
      dramResps.consume
      dramRespBeatCount <== dramRespBeatCount.val + 1
      -- At end of burst, increment completed fetches
      when (dramRespBeatCount.val .==. ones) do
        incrBy fetchDoneCount 1
      -- Completed fetches can never exceed number of inflight requests
      dynamicAssert (inv fetchDoneCount.isFull)
        "StreamCache: broken invariant on fetchDoneCount"

  -- Cache response
  -- ==============

  -- Pulsed when response queue is being consumed
  respConsumeWire :: Wire (Bit 1) <- makeWire false

  -- Track current size of respQueue
  respCount :: Counter (StreamCacheLogMaxInflight+1) <-
    makeCounter (fromInteger (2^StreamCacheLogMaxInflight))

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

  -- Respond with zero
  respZero :: Reg (Bit 1) <- makeReg dontCare

  -- Stage 1: access data memory
  always do
    let inflight = inflightQueue.first
    let req = inflight.inflightReq
    -- Determine address for access to cache's data memory
    let dataMemAddr = inflight.inflightWay #
                        truncate req.streamCacheReqAddr
    -- State 0: data lookup
    when (respState.val .==. 0) do
      when (inflightQueue.canDeq) do
        -- Stall condition
        let stall = -- Line has not yet been replaced
                    inflight.inflightReplace .&&.
                      fetchDoneCount.getCount .==. 0
               .||. -- Response buffer is full
                    respCount.isFull .&&. inv respConsumeWire.val
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
              respZero <== inflight.inflightZero
          -- Decrement fetch count on replacement
          when (inflight.inflightReplace) do
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
            fromBitList
              [ cond ? (new, old)
              | (cond, old, new) <-
                  zip3 (toBitList $ pack mask)
                       (toBitList $ dataMemB.outBE)
                       (toBitList $ pack newData) ]
      let writeZeroes :: DRAMBeat =
            fromBitList
              [ cond ? (0, old)
              | (cond, old) <-
                  zip (toBitList $ req.streamCacheReqZeroBits.val)
                      (toBitList $ dataMemB.outBE) ]
      -- Update data memory
      storeBE dataMemB
        dataMemAddr
        ones
        (if req.streamCacheReqZeroBits.valid then writeZeroes else writeData)
      -- Move back to initial state
      respState <== 0

  -- Stage 2: issue response
  always do
    when (issueResp.val) do
      -- Ensure response queue has space
      dynamicAssert (respQueue.notFull) "StreamCache: response queue overflow"
      -- Split response item items
      let dataItems :: V.Vec (2^StreamCacheLogItemsPerBeat) StreamCacheData =
            unpack dataMemB.outBE
      -- Enqueue response
      enq respQueue
        StreamCacheResp {
          streamCacheRespId = respIdReg.val
        , streamCacheRespData =
            respZero.val ? (zero, dataItems ! respBeatOffset.val)
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
    , toStream dramReqQueue
    )
