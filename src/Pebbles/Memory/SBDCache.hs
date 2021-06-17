-- Simple blocking data cache (SBDCache)

module Pebbles.Memory.SBDCache
  ( makeSBDCache
  ) where

-- SoC parameters
#include <Config.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.Stream
import Blarney.SourceSink
import qualified Blarney.Vector as V

-- Pebbles imports
import Pebbles.Memory.Interface
import Pebbles.Memory.Alignment
import Pebbles.Memory.DRAM.Interface

-- Types
-- =====

-- | DRAM request ids from the cache are unused
type DRAMReqId = ()

-- | Cache line tag width
type TagWidth = 32 - (SBDCacheLogLines + DRAMBeatLogBytes)

-- | Cache line tag (upper bits of address)
type Tag = Bit TagWidth

-- | Cache line number (lower bits of address)
type LineNum = Bit SBDCacheLogLines

-- | Cache line state
data LineState =
  LineState {
    -- | Tag (upper bits of address)
    lineTag :: Tag
    -- | Valid bit
  , lineValid :: Bit 1
    -- | Dirty bit (true if line has been modified)
  , lineDirty :: Bit 1
  } deriving (Generic, Bits)

-- Helper functions
-- ================

-- | Get tag portion of address
getTag :: Bit 32 -> Tag
getTag addr = tag
  where (tag :: Tag, rest) = split addr

-- | Get line number from address
getLineNum :: Bit 32 -> LineNum
getLineNum addr = upper rest
  where (tag :: Tag, rest) = split addr

-- | Get word offset within line from address
getLineWordOffset :: Bit 32 -> Bit (DRAMBeatLogBytes-2)
getLineWordOffset addr = truncate (slice @31 @2 addr)

-- | Get tag bit offset within line from address
getLineTagBitOffset :: Bit 32 -> Bit (DRAMBeatLogBytes-LogBytesPerTagBit)
getLineTagBitOffset addr =
  truncate (slice @31 @LogBytesPerTagBit addr)

-- Implementation
-- ==============

-- | Create a simple blocking data cache (SBDCache).  It's a
-- direct-mapped writeback cache.  Latency: two cycles for a cache
-- hit.  Throughput: 50%, i.e. a request that hits can be consumed
-- every other cycle.
makeSBDCache :: forall t_id. Bits t_id =>
     -- | Inputs: responses from DRAM
     Stream (DRAMResp DRAMReqId)
     -- | Outputs: MemUnit and a stream of DRAM requests
  -> Module (MemUnit t_id, Stream (DRAMReq DRAMReqId))
makeSBDCache dramResps = do
  -- Data memory (block RAM with byte enables)
  dataMem :: RAMBE SBDCacheLogLines DRAMBeatBytes <- makeDualRAMBE

  -- Meta-data memory
  metaMem :: RAM LineNum LineState <- makeDualRAM

  -- Data tag bits (not to be confused with cache line tag)
  tagBitsMem :: RAM LineNum (Bit TagBitsPerBeat) <-
    if EnableCHERI == 1
      then makeDualRAM
      else return nullRAM

  -- Request wire
  reqWire :: Wire (MemReq t_id) <- makeWire dontCare

  -- Request register
  reqReg :: Reg (MemReq t_id) <- makeReg dontCare

  -- Cache response queue
  respQueue :: Queue (MemResp t_id) <- makeShiftQueue 1

  -- DRAM request queue
  dramReqQueue :: Queue (DRAMReq DRAMReqId) <- makeShiftQueue 1

  -- Cache state
  -- 0: Meta-data & data lookup
  -- 1: Respond on hit, writeback on miss
  -- 2: Miss: fetch new line
  -- 3: Miss: receive new line
  -- 4: Loopback to state 1
  state :: Reg (Bit 3) <- makeReg 0

  always do
    -- Tag & data lookup
    when (state.val .==. 0) do
      load metaMem (reqWire.val.memReqAddr.getLineNum)
      loadBE dataMem (reqWire.val.memReqAddr.getLineNum)
      load tagBitsMem (reqWire.val.memReqAddr.getLineNum)

    -- Extract address components from request register
    let tag = reqReg.val.memReqAddr.getTag
    let lineNum = reqReg.val.memReqAddr.getLineNum
    let lineOffset = reqReg.val.memReqAddr.getLineWordOffset
    let tagBitOffset = reqReg.val.memReqAddr.getLineTagBitOffset

    -- Respond on hit, writeback on miss
    when (state.val .==. 1) do
      -- What kind of request is it
      let isLoad = reqReg.val.memReqOp .==. memLoadOp
      let isFence = reqReg.val.memReqOp .==. memGlobalFenceOp
      let isFlush = reqReg.val.memReqOp .==. memCacheFlushOp
      -- Is it a cache hit?
      let isHit = metaMem.out.lineValid .&&.
                    tag .==. metaMem.out.lineTag .&&.
                      isFlush.inv
      -- Tag bits in loaded cache line
      let tagBits :: V.Vec TagBitsPerBeat (Bit 1) = unpack (tagBitsMem.out)
      -- Separate behaviours for hit and miss
      if isHit .||. isFence
        then do
          -- Handle hit
          -- ==========

          -- Load hit or store hit?
          if isLoad .||. isFence
            then do
              -- Handle load hit
              -- Words in loaded cache line
              let loadWords :: V.Vec DRAMBeatWords (Bit 32) =
                    unpack (dataMem.outBE)
              -- Loaded word
              let loadWord = loadWords ! lineOffset
              -- Tag bit for loaded word
              let loadTagBit = tagBits ! tagBitOffset
              -- Check for space in response queue
              if respQueue.notFull 
                then do
                  -- Formulate load response
                  let loadResp =
                        MemResp {
                          memRespId = reqReg.val.memReqId
                        , memRespData = loadMux loadWord
                            (reqReg.val.memReqAddr.truncate)
                            (reqReg.val.memReqAccessWidth)
                            (reqReg.val.memReqIsUnsigned)
                        , memRespDataTagBit = loadTagBit
                        }
                  -- Issue loadResponse
                  enq respQueue loadResp
                  -- Move back to initial state
                  state <== 0
                else do
                  -- Preserve RAM outputs until response queue ready
                  metaMem.preserveOut
                  dataMem.preserveOutBE
                  tagBitsMem.preserveOut
            else do
              -- Handle store hit
              -- Data to store
              let storeWord = writeAlign (reqReg.val.memReqAccessWidth)
                                         (reqReg.val.memReqData)
              let storeWords :: V.Vec DRAMBeatWords (Bit 32) =
                    V.replicate storeWord
              -- Byte enable for word being written
              let wordBE = genByteEnable (reqReg.val.memReqAccessWidth)
                                         (reqReg.val.memReqAddr)
              let wordsBE :: V.Vec DRAMBeatWords (Bit 4) = V.replicate wordBE
              -- Byte enable for full cache line
              let maskBE be i = (lineOffset .==. fromInteger i) ? (be, 0)
              let lineBE :: Bit DRAMBeatBytes =
                    pack (V.zipWith maskBE wordsBE V.genVec)
              -- Perform store
              storeBE dataMem lineNum lineBE (pack storeWords)
              -- Function to determine the i'th tag bit of the cache line
              let genTagBit i tagBit =
                    if tagBitOffset .==. fromInteger i 
                    then reqReg.val.memReqDataTagBit
                    else tagBit
              -- Compute new tag bits
              let tagBitsNew = V.zipWith genTagBit V.genVec tagBits
              -- Perform store of tag bits
              store tagBitsMem lineNum (pack tagBitsNew)
              -- Set dirty bit
              let line =
                    LineState {
                      lineTag = tag
                    , lineValid = true
                    , lineDirty = true
                    }
              store metaMem lineNum line
              -- Move back to initial state
              state <== 0
        else do
          -- Handle miss
          -- ===========

          -- Check if DRAM request possible
          if dramReqQueue.notFull
            then do
              -- Prepare writeback request
              let dramReq =
                    DRAMReq {
                      dramReqId = ()
                    , dramReqIsStore = true
                    , dramReqAddr = metaMem.out.lineTag # lineNum
                    , dramReqData = dataMem.outBE
                    , dramReqDataTagBits = tagBitsMem.out.fromTagBits
                    , dramReqByteEn = ones
                    , dramReqBurst = 1
                    , dramReqIsFinal = true
                    }
              -- Issue writeback
              when (metaMem.out.lineValid .&. metaMem.out.lineDirty) do
                enq dramReqQueue dramReq
              -- On flush, invalidate line
              when isFlush do
                let line =
                      LineState {
                        lineTag = dontCare
                      , lineValid = false
                      , lineDirty = false
                      }
                store metaMem lineNum line
              -- Move to fetch state (if not flushing)
              state <== isFlush ? (0, 2)
            else do
              -- Preserve RAM outputs until writeback possible
              metaMem.preserveOut
              dataMem.preserveOutBE
              tagBitsMem.preserveOut

    -- Fetch new line
    when (state.val .==. 2) do
      -- Wait until DRAM request possible
      when (dramReqQueue.notFull) do
        -- Prepare fetch request
        let dramReq =
              DRAMReq {
                dramReqId = ()
              , dramReqIsStore = false
              , dramReqAddr = truncate $
                  slice @31 @DRAMBeatLogBytes (reqReg.val.memReqAddr)
              , dramReqData = dontCare
              , dramReqDataTagBits = dontCare
              , dramReqByteEn = 0
              , dramReqBurst = 1
              , dramReqIsFinal = true
              }
        -- Issue fetch
        enq dramReqQueue dramReq
        -- Move to DRAM response state
        state <== 3

    -- Receive DRAM response
    when (state.val .==. 3) do
      -- Wait until DRAM response available
      when (dramResps.canPeek) do
        dramResps.consume
        -- Update cache (if not a fence)
        let line =
              LineState {
                lineTag = tag
              , lineValid = true
              , lineDirty = false
              }
        when (reqReg.val.memReqOp .!=. memGlobalFenceOp) do
          store metaMem lineNum line
          storeBE dataMem lineNum ones (dramResps.peek.dramRespData)
          store tagBitsMem lineNum
            (dramResps.peek.dramRespDataTagBits.toTagBits)
        -- Move to loopback state
        state <== 4

    -- Loopback to state 1
    when (state.val .==. 4) do
      load metaMem lineNum
      loadBE dataMem lineNum
      load tagBitsMem lineNum
      state <== 1

  return
    ( MemUnit {
        memReqs =
          Sink {
            canPut = state.val .==. 0
          , put = \req -> do
              reqWire <== req
              reqReg <== req
              state <== (req.memReqOp .==. memGlobalFenceOp) ? (2, 1)
              -- Checks
              dynamicAssert (req.memReqOp .!=. memAtomicOp)
                "Atomics not yet supported by SBDCache"
              dynamicAssert (req.memReqOp .!=. memLocalFenceOp)
                "Local fence not supported by SBDCache"
          }
      , memResps = respQueue.toStream
      }
    , dramReqQueue.toStream )
