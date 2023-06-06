-- Simple coalescing unit connecting SIMT lanes to DRAM and banked SRAMs

module Pebbles.Memory.CoalescingUnit 
  ( makeCoalescingUnit
  , CoalUnitOptions(..)
  , CoalUnitPerfStats(..)
  ) where

-- SoC parameters
#include <Config.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.Option
import Blarney.PulseWire
import Blarney.SourceSink
import Blarney.Interconnect
import qualified Blarney.Vector as V

-- Pebbles imports
import Pebbles.Util.List
import Pebbles.Util.Counter
import Pebbles.Pipeline.Interface
import Pebbles.Pipeline.SIMT.RegFile
import Pebbles.Memory.Interface
import Pebbles.Memory.Alignment
import Pebbles.Memory.DRAM.Interface

-- Haskell imports
import Data.List
import Data.Proxy
import Control.Monad (forM_)

-- Coalescing Unit Types
-- =====================

-- | DRAM request ids from the coalescing unit are unused
type DRAMReqId = ()

-- | Info for inflight DRAM requests (internal to this module)
data CoalescingInfo t_id =
  CoalescingInfo {
    coalInfoUseSameBlock :: Bit 1
    -- ^ Use the SameBlock stategy?  (Otherwise, use SameAddress strategy)
  , coalInfoMask :: Bit SIMTLanes
    -- ^ Coalescing mask (lanes participating in coalesced access)
  , coalInfoReqId :: t_id
    -- ^ Request id
  , coalInfoTagBitMask :: Bit SIMTLanes
    -- ^ Tag bit mask for each lane
  , coalInfoSameBlockMode :: Bit 2
    -- ^ Mode for SameBlock strategy
  , coalInfoAddr :: Bit DRAMBeatLogBytes
    -- ^ Lower bits of address
  , coalInfoBurstLen :: DRAMBurst
    -- ^ Burst length
  , coalInfoIsFinal :: Bit 1
    -- ^ Final request in transaction?
  , coalInfoStoreBufferHit :: Option (ScalarVal 33)
    -- ^ Is it a load hit in the store buffer?
  } deriving (Generic, Bits)

-- | Coalescing unit parameters/options
data CoalUnitOptions =
  CoalUnitOptions {
    enableStoreBuffer :: Bool
    -- ^ Enable scalarised vector store buffer?
  , isSRAMAccess :: MemReq -> Bit 1
    -- ^ Predicate to determine if request is for SRAM (true) or DRAM (false)
  , canBuffer :: MemReq -> Bit 1
    -- ^ If DRAM, are we allowed to buffer the given request?
  }

-- | For performance stats
data CoalUnitPerfStats =
  CoalUnitPerfStats {
    incLoadHit :: Bit 1
    -- ^ Pulsed on load hit to store buffer
  , incLoadMiss :: Bit 1
    -- ^ Pulsed on load miss to store buffer
  , isCapMetaAccess :: Bit 1
    -- ^ Pulsed on cap meta data access to store buffer
  }
  deriving (Generic, Bits, Interface)

-- Implementation
-- ==============

-- | The coalescing unit takes memory requests from multiple SIMT
-- lanes and coalesces them where possible into a single memory request
-- using two strategies: (1) SameBlock: multiple accesses to the same
-- DRAM block, where the lower bits of each address equal the SIMT
-- lane id, are satisfied by a single DRAM burst; (2) SameAddress:
-- mutliple accesses to the same address are satisifed by a single
-- DRAM/SRAM access. We employ a seven stage pipeline:
--
--   0. Consume requests and feed pipeline
--   1. Pick one SIMT lane as a leader 
--   2. Determine the leader's request with a mux
--   3. Evaluate coalescing strategies
--   4. Choose coalescing strategy and feed back unsatisifed reqs to stage 1
--   5. Issue DRAM/SRAM requests
--   6. Consume DRAM/SRAM responses and issue load responses
--
-- Optionally, an *experimental* scalarised vector store buffer can be
-- enabled, which reduces DRAM overhead for register spills.
-- Currently, the store buffer only buffers compressed uniform
-- vectors. Futhermore, it deatomises multi-flit transactions: when
-- CHERI is enabled, buffered capability stores are non-atomic.
-- However, the kind of stores that can be buffered is parameterised,
-- allowing loss of atomicity to be restricted to regions of memory
-- that are not shared, such as stacks.
--
-- Notes:
--   * The number of SIMT lanes is assumed to be equal to the
--     number of half-words in a DRAM Beat.
--   * We assume that DRAM responses come back in order.
--   * Backpressure on memory responses currently propagates to
--     DRAM responses, which could cause blocking on the DRAM bus.
--   * A global fence is treated like a load (the response data is 
--     ignored but the response signifies that all preceeding stores
--     have reached DRAM).
--   * For capability accesses, the SameBlock strategy is currently
--     only effective when accessing the SIMT stacks.
--   * A global fence does not currently flush the store buffer.
makeCoalescingUnit :: Bits t_id =>
     CoalUnitOptions
     -- ^ Coalescing unit options/parameters
  -> Stream (t_id, V.Vec SIMTLanes (Option MemReq), Option (ScalarVal 33))
     -- ^ Stream of memory requests vectors, plus a compressed write
     -- vector (if data being written is scalarisable)
  -> Stream (DRAMResp DRAMReqId)
     -- ^ Responses from DRAM
  -> Stream (t_id, V.Vec SIMTLanes (Option MemResp))
     -- ^ Responses from SRAM, per lane/bank
  -> Module ( Stream (t_id, V.Vec SIMTLanes (Option MemResp))
            , Stream ( t_id
                     , V.Vec SIMTLanes (Option MemReq)
                     , Option MemReq )
            , Stream (DRAMReq DRAMReqId)
            , CoalUnitPerfStats
            )
     -- ^ Outputs:
     --     (1) memory responses per lane;
     --     (2) SRAM requests per lane/bank, plus leader request,
     --         valid when all requests access same address;
     --     (3) DRAM requests
makeCoalescingUnit opts memReqsStream dramResps sramResps = do
  -- Assumptions
  staticAssert (SIMTLanes == DRAMBeatHalfs)
    ("Coalescing Unit: number of SIMT lanes must equal " ++
     "number of half-words in DRAM beat")

  -- Trigger signals for each pipeline stage
  go1 :: Reg (Bit 1) <- makeDReg false
  go2 :: Reg (Bit 1) <- makeDReg false
  go3 :: Reg (Bit 1) <- makeDReg false
  go4 :: Reg (Bit 1) <- makeDReg false
  go5 :: Reg (Bit 1) <- makeReg false

  -- Requests for each pipeline stage
  memReqs1 :: [Reg MemReq] <- replicateM SIMTLanes (makeReg dontCare)
  memReqs2 :: [Reg MemReq] <- replicateM SIMTLanes (makeReg dontCare)
  memReqs3 :: [Reg MemReq] <- replicateM SIMTLanes (makeReg dontCare)
  memReqs4 :: [Reg MemReq] <- replicateM SIMTLanes (makeReg dontCare)
  memReqs5 :: [Reg MemReq] <- replicateM SIMTLanes (makeReg dontCare)

  -- Request ids for each pipeline stage
  reqId1 :: Reg t_id <- makeReg dontCare
  reqId2 :: Reg t_id <- makeReg dontCare
  reqId3 :: Reg t_id <- makeReg dontCare
  reqId4 :: Reg t_id <- makeReg dontCare
  reqId5 :: Reg t_id <- makeReg dontCare

  -- Scalarised write vector for each pipeline stage
  scalarVal1 :: Reg (Option (ScalarVal 33)) <- makeReg dontCare
  scalarVal2 :: Reg (Option (ScalarVal 33)) <- makeReg dontCare
  scalarVal3 :: Reg (Option (ScalarVal 33)) <- makeReg dontCare
  scalarVal4 :: Reg (Option (ScalarVal 33)) <- makeReg dontCare

  -- Pending request mask for each pipeline stage
  pending1 :: Reg (Bit SIMTLanes) <- makeReg dontCare
  pending2 :: Reg (Bit SIMTLanes) <- makeReg dontCare
  pending3 :: Reg (Bit SIMTLanes) <- makeReg dontCare
  pending4 :: Reg (Bit SIMTLanes) <- makeReg dontCare

  -- Leader requests for each pipeline stage
  leaderReq3 :: Reg MemReq <- makeReg dontCare
  leaderReq4 :: Reg MemReq <- makeReg dontCare
  leaderReq5 :: Reg MemReq <- makeReg dontCare

  -- Bit vector identifying the chosen leader
  leader2 :: Reg (Bit SIMTLanes) <- makeReg 0
  leader3 :: Reg (Bit SIMTLanes) <- makeReg 0
  leader4 :: Reg (Bit SIMTLanes) <- makeReg 0

  -- Currently processing multi-flit transaction
  multiFlit4 <- makeReg false

  -- DRAM request queue
  dramReqQueue :: Queue (DRAMReq DRAMReqId) <- makePipelineQueue 1

  -- Inflight DRAM requests
  inflightQueue :: Queue (CoalescingInfo t_id) <-
    makeSizedQueue DRAMLogMaxInFlight

  -- DRAM response queue
  dramRespQueue :: Queue (t_id, V.Vec SIMTLanes (Option MemResp)) <-
    makeShiftQueue 1

  -- Scalarised vector store buffer (SVSB)
  storeBuffer :: (RAM SVSBIndex SVSBEntry) <-
    if opts.enableStoreBuffer
      then makeDualRAMForward
      else return nullRAM

  -- Enable store buffer?
  let enStoreBuffer = if opts.enableStoreBuffer then true else false

  -- Performance stats
  incStoreBufferLoadHit <- makePulseWire
  incStoreBufferLoadMiss <- makePulseWire
  isCapMetaAccess <- makePulseWire

  -- Stage 0: consume requests and feed pipeline
  -- ===========================================

  -- Pipeline feedback trigger from stage 3
  feedbackWire :: Wire (Bit 1) <- makeWire false

  -- Pipeline stall wire
  stallWire :: Wire (Bit 1) <- makeWire false

  -- Track number of flits in 4-stage pipeline
  inflightCount :: Counter 3 <- makeCounter 4

  -- Multi-flit transaction currenlty being inserted into the pipeline?
  partialInsert :: Reg (Bit 1) <- makeReg false

  -- Multi-flit transaction currenlty being fed back?
  partialFeedback :: Reg (Bit 1) <- makeReg false

  always do
    -- Invariant: feedback and stall never occur together)
    dynamicAssert (inv (feedbackWire.val .&. stallWire.val))
      "Coalescing Unit: feedback and stall both high"

    -- Memory requests to consume
    let memReqs = V.toList (memReqsStream.peek._1)

    -- Is this the final flit of a multi-flit transaction?
    let isFinal = orList [ r.valid .&&. r.val.memReqIsFinal
                         | r <- memReqs ]

    -- Is there space for two flits?
    let spaceForTwo = isFinal ? (true, inflightCount.getAvailable .>=. 2)

    -- Inject requests from input queues when no stall/feedback in progress
    when (memReqsStream.canPeek) do
      when (inv partialFeedback.val .&&.
              spaceForTwo .&&.
                (inv stallWire.val .||. inv go1.val) .&&.
                  inv feedbackWire.val) do
        -- Consume next vector of requests
        memReqsStream.consume
        -- Inject into pipeline
        forM_ (zip memReqs memReqs1) \(req, reg) -> do
          reg <== req.val
        -- Initialise pending mask
        pending1 <== fromBitList [r.valid | r <- memReqs]
        -- Trigger pipeline
        go1 <== true
        reqId1 <== memReqsStream.peek._0
        scalarVal1 <== memReqsStream.peek._2
        incrBy inflightCount 1
        partialInsert <== inv isFinal

    -- Preserve go signals on stall
    when (stallWire.val) do
      when (go3.val) do go3 <== true
      when (go4.val) do go4 <== true

  -- Stage 1: Pick a leader
  -- ======================

  always do
    when (go1.val) do
      if go2.val .&&. stallWire.val
        then go1 <== true
        else do
          -- Select first pending request as leader
          leader2 <== pending1.val .&. (inv pending1.val + 1)
          -- Trigger stage 2
          go2 <== true
          zipWithM_ (<==) memReqs2 (map (.val) memReqs1)
          pending2 <== pending1.val
          reqId2 <== reqId1.val
          scalarVal2 <== scalarVal1.val

  -- Stage 2: Select leader's request
  -- ================================

  always do

    when (go2.val) do
      if go3.val .&&. stallWire.val
        then go2 <== true
        else do
          -- There must exist at least one possible leader
          dynamicAssert (leader2.val .!=. 0)
            "Coalescing Unit (Stage 2): no leader found"
          -- Mux to select leader's request
          let leaderReq = select (zip (toBitList leader2.val)
                                      (map (.val) memReqs2))
          leaderReq3 <== leaderReq
          -- Trigger stage 3
          go3 <== true
          zipWithM_ (<==) memReqs3 (map (.val) memReqs2)
          pending3 <== pending2.val
          reqId3 <== reqId2.val
          scalarVal3 <== scalarVal2.val
          leader3 <== leader2.val

  -- Stage 3: Evaluate coalescing strategies
  -- =======================================

  -- Outcome of stage 3
  sameBlockMode4  :: Reg (Bit 2) <- makeReg dontCare
  sameBlockMask4  :: Reg (Bit SIMTLanes) <- makeReg dontCare
  sameAddrMask4   :: Reg (Bit SIMTLanes) <- makeReg dontCare
  sramMask4       :: Reg (Bit SIMTLanes) <- makeReg dontCare
  isSRAMAccess4   :: Reg (Bit 1) <- makeReg dontCare
  bufferable4     :: Reg (Bit 1) <- makeReg dontCare
  canBuffer4      :: Reg (Bit 1) <- makeReg dontCare
  storeBufferIdx4 :: Reg SVSBIndex <- makeReg dontCare

  always do
    -- We assume that inputs to the coalescing unit have passed
    -- through the warp preserver, which means that all requests being
    -- processed simultaneously have arisen from the same instruction.
    -- This implies that they all have the same access width and
    -- memory operation.
    let sameOpAndAccessWidth = andList
          [ valid .==>.
              (leaderReq3.val.memReqAccessWidth .==. req.val.memReqAccessWidth
                .&&. leaderReq3.val.memReqOp .==. req.val.memReqOp
                .&&. leaderReq3.val.memReqIsFinal .==. req.val.memReqIsFinal)
          | (req, valid) <- zip memReqs3 (toBitList pending3.val) ]
    when (go3.val) do
      dynamicAssert sameOpAndAccessWidth
        "Coalescining unit: requests have different op or access width"

    -- Which requests can be satisfied by SameAddr/SameBlock strategies?
    -- -----------------------------------------------------------------

    -- There are three ways to satisfy the SameBlock Strategy: either
    -- the requests access a contiguous array of bytes (ByteMode=0),
    -- half words (HalfMode=1), or words (WordMode=2).  In ByteMode,
    -- the access width of each request must be a byte; in HalfMode,
    -- it must be a half word; in WordMode, it can be anything
    -- (as long as it is consistent).  This feature of WordMode
    -- allows efficient sub-word stack access, where stacks are
    -- interleaved at the word level.
    let sameBlockMatch req (laneId :: Bit SIMTLogLanes) =
            [ sameBlock .&&. sameAddr
            , sameBlock .&&. byteMatch
            , sameBlock .&&. halfMatch
            , sameBlock .&&. wordMatch ]
          where
            a1 = req.memReqAddr
            a2 = leaderReq3.val.memReqAddr
            aw = leaderReq3.val.memReqAccessWidth
            sameBlock =
              slice @31 @(SIMTLogLanes+2) a1 .==.
                slice @31 @(SIMTLogLanes+2) a2
            sameAddr  = slice @(SIMTLogLanes+1) @0 a1 .==.
                          slice @(SIMTLogLanes+1) @0 a2
            byteMatch = slice @(SIMTLogLanes-1) @0 a1 .==. laneId
                   .&&. slice @(SIMTLogLanes+1) @SIMTLogLanes a1 .==.
                          slice @(SIMTLogLanes+1) @SIMTLogLanes a2
            halfMatch = slice @SIMTLogLanes @1 a1 .==. laneId
                   .&&. at @(SIMTLogLanes+1) a1 .==. at @(SIMTLogLanes+1) a2
            wordMatch = slice @1 @0 a1 .==. slice @1 @0 a2
                   .&&. slice @(SIMTLogLanes+1) @2 a1 .==. laneId

    -- Which requests satisfy each SameBlock mode?
    let sameBlockMasks :: [Bit SIMTLanes] =
          map fromBitList $ transpose
            [ sameBlockMatch r (fromInteger i)
            | (r, i) <- zip (map (.val) memReqs3) [0..] ]

    -- Take into account which requests are valid
    let [sameAddrMaskVal, byteModeMask, halfModeMask, wordModeMask] =
          map (pending3.val .&.) sameBlockMasks

    -- Requests destined for banked SRAMs
    let sramMask = 
          [ p .&. opts.isSRAMAccess r
          | (p, r) <- zip (toBitList pending3.val) (map (.val) memReqs3) ]

    -- Lookup store buffer
    let storeBufferAddr :: DRAMVecAddr =
          stallWire.val ? ( upper leaderReq4.val.memReqAddr
                          , upper leaderReq3.val.memReqAddr )
    storeBuffer.load (lower storeBufferAddr)

    -- State update
    when (go3.val .&. inv stallWire.val) do
      -- SameAddress strategy should at least allow the leader to progress
      dynamicAssert (sameAddrMaskVal .!=. 0)
        "Coalescing Unit: SameAddr strategy does not make progress!"
      -- Requests satisifed by SameAddress strategy
      sameAddrMask4 <== sameAddrMaskVal
      -- For SameBlock strategy, choose WordMode if it satisfies leader
      -- and at least one other request
      let useWordMode = (wordModeMask .&. leader3.val .!=. 0) .&.
                          (wordModeMask .&. inv leader3.val .!=. 0)
                   .||. isWordAccess leaderReq3.val.memReqAccessWidth
      if useWordMode
        then do
          sameBlockMode4 <== 2
          sameBlockMask4 <== wordModeMask
        else do
          -- Otherwise, use access width to determine mode
          if isHalfAccess leaderReq3.val.memReqAccessWidth
            then do
              sameBlockMode4 <== 1
              sameBlockMask4 <== halfModeMask
            else do
              sameBlockMode4 <== 0
              sameBlockMask4 <== byteModeMask
      -- Is leader accessing banked SRAMs?
      isSRAMAccess4 <== opts.isSRAMAccess leaderReq3.val
      sramMask4 <== fromBitList sramMask
      -- Can we insert into store buffer?
      let canBuffer = opts.canBuffer leaderReq3.val
      bufferable4 <== andList
        [ -- Store buffer is enabled
          enStoreBuffer
          -- It's a store
        , leaderReq3.val.memReqOp .==. memStoreOp
          -- Write vector is scalarisable
        , scalarVal3.val.valid
          -- Write vector is uniform
        , scalarVal3.val.val.stride .==. stride_0
          -- Is the access bufferable?
        , canBuffer
          -- SameBlock strategy, with all lanes active
        , wordModeMask .==. ones
          -- SameBlock WordMode is in operation
        , useWordMode
          -- Access width is 4 bytes
        , isWordAccess leaderReq3.val.memReqAccessWidth
        ]
      canBuffer4 <== canBuffer
      storeBufferIdx4 <== lower storeBufferAddr
      -- Trigger stage 4
      go4 <== true
      zipWithM_ (<==) memReqs4 (map (.val) memReqs3)
      pending4 <== pending3.val
      reqId4 <== reqId3.val
      scalarVal4 <== scalarVal3.val
      leader4 <== leader3.val
      leaderReq4 <== leaderReq3.val

  -- Stage 4: Choose coalescing strategy
  -- ===================================

  -- Choice of strategy
  coalSameBlockStrategy :: Reg (Bit 1) <- makeReg dontCare

  -- Mode for SameBlock strategy
  coalSameBlockMode :: Reg (Bit 2) <- makeReg dontCare

  -- Which lanes are participating in the strategy
  coalMask :: Reg (Bit SIMTLanes) <- makeReg dontCare

  -- Requests to banked SRAMs
  sramReqs :: Queue ( t_id
                    , V.Vec SIMTLanes (Option MemReq)
                    , Option MemReq ) <- makeShiftQueue 2

  -- Store buffer load hit?
  coalStoreBufferLoadHit :: Reg (Option (ScalarVal 33)) <- makeReg dontCare

  -- Wire goes high when store buffer eviction is required
  storeBufferEvict :: Wire (Bit 1) <- makeWire false

  always do
    -- Use SameBlock strategy if it satisfies leader's request and at
    -- least one other request.  Otherwise use SameAddr strategy,
    -- which will always satisfy at least one request.
    let useSameBlock =
          ((sameBlockMask4.val .&. leader4.val) .==. leader4.val) .&.
            ((sameBlockMask4.val .&. inv leader4.val) .!=. 0)
    -- Requests participating in strategy
    let mask = isSRAMAccess4.val ? (sramMask4.val,
                 useSameBlock ? (sameBlockMask4.val, sameAddrMask4.val))
    -- Try to trigger next stage
    when (go4.val) do
      let busy = isSRAMAccess4.val ? (inv sramReqs.notFull, go5.val)
      -- Stall if stage 5 is currently busy
      -- or transaction currently being inserted
      if busy .||. partialInsert.val
        then do
          -- If so, stall pipeline
          stallWire <== true
        else do
          -- Otherwise, setup and trigger next stage
          if isSRAMAccess4.val
            then do
              let reqs = V.fromList
                           [ Option active req
                           | (req, active) <- zip (map (.val) memReqs4)
                                                  (toBitList sramMask4.val) ]
              -- Use same address strategy if all SRAM accesses are
              -- loads to the same address
              let useSameAddr =
                    leaderReq4.val.memReqOp .==. memLoadOp .&&.
                      sramMask4.val .==. sameAddrMask4.val
              let leader = Option useSameAddr (leaderReq4.val)
              enq sramReqs (reqId4.val, reqs, leader)
            else do
              multiFlit4 <== inv leaderReq4.val.memReqIsFinal
              -- Check that atomics are not in use
              dynamicAssert (leaderReq4.val.memReqOp .!=. memAtomicOp)
                "Atomics not yet supported on DRAM path"
              -- Check for a store buffer hit
              let isStoreBufferHit = andList
                    [ -- Store buffer is enabled
                      enStoreBuffer
                      -- Store buffer entry is valid
                    , storeBuffer.out.valid
                      -- It's a hit
                    , storeBuffer.out.tag .==. upper leaderReq4.val.memReqAddr
                    ]
              -- Check for store buffer load hit
              let isStoreBufferLoadHit = isStoreBufferHit .&&.
                    leaderReq4.val.memReqOp .==. memLoadOp
              when isStoreBufferLoadHit do
                incStoreBufferLoadHit.pulse
              when (inv isStoreBufferLoadHit .&&. canBuffer4.val .&&.
                      leaderReq4.val.memReqOp .==. memLoadOp) do
                incStoreBufferLoadMiss.pulse
              when multiFlit4.val do
                isCapMetaAccess.pulse
              coalStoreBufferLoadHit <==
                Option isStoreBufferLoadHit storeBuffer.out.scalarVal
              -- Check store buffer
              when (leaderReq4.val.memReqOp .==. memStoreOp) do
                if isStoreBufferHit
                  then do
                    when (inv bufferable4.val) do
                      -- Evict old store buffer entry
                      storeBufferEvict <== true
                      -- Stall for eviction
                      stallWire <== true
                      -- Invalidate store buffer entry
                      storeBuffer.store storeBufferIdx4.val
                        SBEntry {
                          valid = false
                        , tag = dontCare
                        , scalarVal = dontCare
                        }
                  else do
                    when bufferable4.val do
                      -- Evict old store buffer entry if it's valid
                      when storeBuffer.out.valid do
                        storeBufferEvict <== true
              -- Overwrite store buffer entry
              when bufferable4.val do
                storeBuffer.store storeBufferIdx4.val
                  SBEntry {
                    valid = true
                  , tag = upper leaderReq4.val.memReqAddr
                  , scalarVal = scalarVal4.val.val
                  }
              -- Trigger next stage
              -- (unless it's non-evicting store)
              let nonEvictingStore =
                    leaderReq4.val.memReqOp .==. memStoreOp .&&.
                      bufferable4.val .&&.
                        inv storeBufferEvict.val
              when (inv enStoreBuffer .||. inv nonEvictingStore) do
                go5 <== true
              -- Evict store buffer, or pass request through
              if storeBufferEvict.val
                then do
                  coalSameBlockStrategy <== true
                  coalSameBlockMode <== 2
                  coalMask <== ones
                  let evictReq = MemReq {
                          memReqAccessWidth = 2   -- 2^2=4 bytes
                        , memReqOp = memStoreOp
                        , memReqAMOInfo = dontCare
                        , memReqAddr = storeBuffer.out.tag #
                            storeBufferIdx4.val # 0
                        , memReqData =
                            lower storeBuffer.out.scalarVal.val
                        , memReqDataTagBit =
                            upper storeBuffer.out.scalarVal.val
                        , memReqDataTagBitMask = dontCare
                        , memReqIsUnsigned = dontCare
                        , memReqIsFinal = true
                        } 
                  leaderReq5 <== evictReq
                  reqId5 <== dontCare
                  forM_ memReqs5 \r5 -> do
                    r5 <== evictReq
                else do
                  coalSameBlockStrategy <== useSameBlock
                  coalSameBlockMode <== sameBlockMode4.val
                  coalMask <== mask
                  reqId5 <== reqId4.val
                  -- Deatomise/split multi-flit transactions?
                  let split = enStoreBuffer .&&. canBuffer4.val .&&.
                        leaderReq4.val.memReqOp .==. memStoreOp
                  leaderReq5 <== split ?
                    ( leaderReq4.val { memReqIsFinal = true }
                    , leaderReq4.val )
                  forM_ (zip memReqs4 memReqs5) \(r4, r5) -> do
                    r5 <== split ?
                      ( r4.val { memReqIsFinal = true }
                      , r4.val )

          -- Deal with remaining pending requests (if there are any)
          when (inv stallWire.val) do
            -- Determine any remaining pending requests
            let remaining = pending4.val .&. inv mask
            -- If there are any, feed them back
            if remaining .==. 0
              then do
                decrBy inflightCount 1
              else do
                go1 <== true
                feedbackWire <== true
                partialFeedback <== inv leaderReq4.val.memReqIsFinal
                zipWithM_ (<==) memReqs1 (map (.val) memReqs4)
                pending1 <== remaining
                reqId1 <== reqId4.val

  -- Stage 5 (DRAM): Issue DRAM requests
  -- ===================================

  -- Count register for burst store
  storeCount :: Reg DRAMBurst <- makeReg 0

  always do
    -- Shorthands for chosen strategy
    let useSameBlock = coalSameBlockStrategy.val
    let sameBlockMode = coalSameBlockMode.val
    let mask = coalMask.val
    -- Determine burst length and address mask (to align the burst)
    let (burstLen, addrMask) :: (DRAMBurst, Bit 1) =
          if useSameBlock
            then
              select [
                isByteAccess sameBlockMode --> (1, 0b0)
              , isHalfAccess sameBlockMode --> (1, 0b0)
              , isWordAccess sameBlockMode --> (2, 0b1)
              ]
            else (1, 0b0)
    -- The DRAM address is derived from the top bits of the memory address
    let dramAddr = slice @31 @DRAMBeatLogBytes (leaderReq5.val.memReqAddr)
    -- DRAM data field for SameBlock strategy
    let sameBlockData8 :: V.Vec DRAMBeatBytes (Bit 8) =
          V.fromList $ concat $ replicate 2 $ concat
            [ [slice @7 @0 (r1.val.memReqData),
               slice @15 @8 (r2.val.memReqData),
               slice @23 @16 (r3.val.memReqData),
               slice @31 @24 (r4.val.memReqData)]
            | [r1, r2, r3, r4] <- groupsOf 4 memReqs5]
    let sameBlockData16 :: V.Vec DRAMBeatHalfs (Bit 16) =
          V.fromList $ concat $
            [ [lower r1.val.memReqData, upper r2.val.memReqData]
            | [r1, r2] <- groupsOf 2 memReqs5 ]
    let sameBlockData32 :: V.Vec DRAMBeatWords (Bit 32) =
          V.fromList $ selectHalf (truncate storeCount.val)
            [r.val.memReqData | r <- memReqs5]
    let sameBlockData :: DRAMBeat =
          [pack sameBlockData8,
             pack sameBlockData16,
               pack sameBlockData32] ! sameBlockMode
    -- Tag bits for SameBlock strategy
    let sameBlockTagBits8 :: Bit DRAMBeatWords =
          fromBitList $ concat $ replicate 2 $
            [ andList $ map memReqDataTagBit $ map (.val) rs
            | rs <- groupsOf 4 memReqs5]
    let sameBlockTagBits16 :: Bit DRAMBeatWords =
          fromBitList
            [ andList $ map memReqDataTagBit $ map (.val) rs
            | rs <- groupsOf 2 memReqs5]
    let sameBlockTagBits32 :: Bit DRAMBeatWords =
          fromBitList $ selectHalf (truncate storeCount.val)
            [r.val.memReqDataTagBit | r <- memReqs5]
    let sameBlockTagBits =
          [sameBlockTagBits8,
             sameBlockTagBits16,
               sameBlockTagBits32] ! sameBlockMode
    -- DRAM data field for SameAddress strategy
    let sameAddrDataVec :: V.Vec DRAMBeatWords (Bit 32) =
          V.replicate (leaderReq5.val.memReqData)
    let sameAddrData :: DRAMBeat = pack sameAddrDataVec
    -- Tag bits for SameAddress strategy
    let sameAddrTagBits :: Bit DRAMBeatWords =
          rep (leaderReq5.val.memReqDataTagBit)
    -- DRAM byte enable field for SameBlock strategy
    let useUpper = at @(DRAMBeatLogBytes-1) (leaderReq5.val.memReqAddr)
    let sameBlockBE8 :: Bit DRAMBeatBytes =
          fromBitList $
            [en .&. inv useUpper | en <- toBitList mask] ++
            [en .&. useUpper | en <- toBitList mask]
    let sameBlockBE16 :: Bit DRAMBeatBytes =
          fromBitList $ concatMap (replicate 2) (toBitList mask)
    let sameBlockBE32 :: Bit DRAMBeatBytes =
          fromBitList $ concatMap toBitList $
            selectHalf (truncate storeCount.val)
              [ rep en .&.
                  genByteEnable
                    (r.val.memReqAccessWidth)
                    (r.val.memReqAddr)
              | (en, r) <- zip (toBitList mask) memReqs5 ]
    let sameBlockBE = [sameBlockBE8, sameBlockBE16, sameBlockBE32] !
           sameBlockMode
    -- DRAM byte enable field for SameAddress strategy
    let leaderBE = genByteEnable (leaderReq5.val.memReqAccessWidth)
                    (leaderReq5.val.memReqAddr)
    let subWord = slice @(DRAMBeatLogBytes-1) @2 (leaderReq5.val.memReqAddr)
    let sameAddrBEVec :: V.Vec DRAMBeatWords (Bit 4) = 
          V.fromList [ subWord .==. fromInteger i ? (leaderBE, 0)
                     | i <- [0..DRAMBeatWords-1] ]
    let sameAddrBE :: Bit DRAMBeatBytes = pack sameAddrBEVec
    -- Is it a DRAM store request?
    let isStore = leaderReq5.val.memReqOp .==. memStoreOp
    -- Is it the final store in a transaction?
    let newStoreCount = storeCount.val + 1
    let isFinalStore = newStoreCount .==. burstLen
    -- Formulate DRAM request
    let dramReq =
          DRAMReq {
            dramReqId = ()
          , dramReqIsStore = isStore
          , dramReqAddr = truncate dramAddr .&. inv (zeroExtend addrMask)
          , dramReqData = useSameBlock ? (sameBlockData, sameAddrData)
          , dramReqDataTagBits =
              useSameBlock ? (sameBlockTagBits, sameAddrTagBits)
          , dramReqByteEn = useSameBlock ? (sameBlockBE, sameAddrBE)
          , dramReqBurst = burstLen
          , dramReqIsFinal =
              isStore ? ( isFinalStore .&&. leaderReq5.val.memReqIsFinal
                        , leaderReq5.val.memReqIsFinal )
          }
    -- Try to issue DRAM request
    when (go5.val) do
      -- Check that we can make a DRAM request
      when (inflightQueue.notFull .&. dramReqQueue.notFull) do
        -- Issue DRAM request (unless store buffer hit)
        when (inv coalStoreBufferLoadHit.val.valid) do
          enq dramReqQueue dramReq
        -- Info needed to process response
        let info =
              CoalescingInfo {
                coalInfoUseSameBlock = useSameBlock
              , coalInfoMask = mask
              , coalInfoReqId = reqId5.val
              , coalInfoTagBitMask =
                  fromBitList [r.val.memReqDataTagBitMask | r <- memReqs5]
              , coalInfoSameBlockMode = sameBlockMode
              , coalInfoAddr = truncate leaderReq5.val.memReqAddr
              , coalInfoBurstLen = burstLen - 1
              , coalInfoIsFinal = leaderReq5.val.memReqIsFinal
              , coalInfoStoreBufferHit = coalStoreBufferLoadHit.val
              }
        -- Handle load & fence: insert info into inflight queue
        let hasResp = leaderReq5.val.memReqOp .==. memLoadOp
                        .||. leaderReq5.val.memReqOp .==. memGlobalFenceOp
        when hasResp do
          enq inflightQueue info
          go5 <== false
        -- Handle store: increment burst count
        when (leaderReq5.val.memReqOp .==. memStoreOp) do
          if isFinalStore
            then do
              storeCount <== 0
              go5 <== false
            else do
              storeCount <== newStoreCount

  -- Stage 6 (DRAM): Handle responses
  -- ================================

  -- Count register for burst load
  loadCount :: Reg DRAMBurst <- makeReg 0

  -- For accumulating responses
  dramRespsAccum :: [Reg MemResp] <-
    replicateM SIMTLanes (makeReg dontCare)
  dramRespsAccumValid :: Reg (Bit SIMTLanes) <- makeReg 0
  dramRespId :: Reg t_id <- makeReg dontCare

  -- This stage can be in one of two states: accumulation and response
  let s_Accum6 :: Bit 1 = 0
  let s_Respond6 :: Bit 1 = 1
  state6 :: Reg (Bit 1) <- makeReg s_Accum6

  always do
    -- Fields needed for response
    let resp = dramResps.peek
    let info = inflightQueue.first
    -- Shorthands for chosen strategy
    let useSameBlock = info.coalInfoUseSameBlock
    let mask = info.coalInfoMask
    -- Shorthand for access info
    let sameBlockMode = info.coalInfoSameBlockMode
    -- Which lanes may deliver a response under SameBlock strategy?
    let deliverSameBlock =
          [ loadCount.val .==. 
              select [
                isByteAccess sameBlockMode --> 0
              , isHalfAccess sameBlockMode --> 0
              , isWordAccess sameBlockMode -->
                  fromInteger (i `div` DRAMBeatWords)
              ]
          | i <- [0..SIMTLanes-1] ]
    -- Which lanes may deliver a response under any strategy?
    let deliverAny = map (useSameBlock .<=.) deliverSameBlock
    -- Consider only those lanes participating in the strategy
    let activeAny = zipWith (.&.) deliverAny (toBitList mask)
    -- Determine items of data response
    let beatBytes :: V.Vec DRAMBeatBytes (Bit 8) = unpack (resp.dramRespData)
    let beatHalfs :: V.Vec DRAMBeatHalfs (Bit 16) = unpack (resp.dramRespData)
    let beatWords :: V.Vec DRAMBeatWords (Bit 32) = unpack (resp.dramRespData)
    -- Response data for SameAddress strategy
    let sameAddrWordIndex :: Bit (DRAMBeatLogBytes-2) =
          upper info.coalInfoAddr
    let sameAddrData = beatWords ! sameAddrWordIndex
    -- Response tag bits for SameAddress strategy
    let sameAddrTagBit = resp.dramRespDataTagBits ! sameAddrWordIndex
    -- Accessing lower or upper bytes?
    let bytesSel = at @(DRAMBeatLogBytes-1) (info.coalInfoAddr)
    -- Response data for SameBlock strategy
    let sameBlockBytes :: V.Vec SIMTLanes (Bit 32) =
          V.fromList $
            map (\x -> x # x # x # x) $
              selectHalf bytesSel $
                V.toList beatBytes
    let sameBlockHalfs :: V.Vec SIMTLanes (Bit 32) =
          V.map (\x -> x # x) beatHalfs
    let sameBlockData :: V.Vec SIMTLanes (Bit 32) =
          [ sameBlockBytes
          , sameBlockHalfs
          , beatWords `V.append` beatWords
          ] ! sameBlockMode
    -- Response tag bits for SameBlock strategy
    let sameBlockByteTagBits :: Bit SIMTLanes =
          fromBitList $
            concatMap (replicate 4) $
              selectHalf bytesSel $
                toBitList resp.dramRespDataTagBits
    let sameBlockHalfTagBits :: Bit SIMTLanes =
          fromBitList $ concat
            [[b, b] | b <- toBitList resp.dramRespDataTagBits]
    let sameBlockTagBits :: Bit SIMTLanes =
          [ sameBlockByteTagBits
          , sameBlockHalfTagBits
          , resp.dramRespDataTagBits # resp.dramRespDataTagBits
          ] ! sameBlockMode
    -- Handle store buffer hit, or DRAM response?
    let hit = inflightQueue.first.coalInfoStoreBufferHit
    if inflightQueue.canDeq .&&. hit.valid
      then do
        when opts.enableStoreBuffer do
          -- Note: store buffer currently restricted to uniform vectors
          when dramRespQueue.notFull do
            let vec = V.fromList
                        [ Option valid
                            MemResp {
                              memRespData = lower hit.val.val
                            , memRespDataTagBit =
                                tMask .&&. upper hit.val.val
                            , memRespIsFinal = info.coalInfoIsFinal
                            }
                        | (valid, tMask) <-
                            zip (toBitList mask)
                                (toBitList info.coalInfoTagBitMask) ]
            dramRespQueue.enq (info.coalInfoReqId, vec)
            inflightQueue.deq
      else do
        -- State machine (accumulate DRAM responses and respond)
        if state6.val .==. s_Respond6
          then do
            when dramRespQueue.notFull do
              -- Enqueue responses
              let vec = V.fromList
                    [ Option valid resp.val
                    | (valid, resp) <- zip (toBitList dramRespsAccumValid.val)
                                           dramRespsAccum ]
              dramRespQueue.enq (dramRespId.val, vec)
              dramRespsAccumValid <== 0
              inflightQueue.deq
              state6 <== s_Accum6
           else do
             -- Condition for consuming DRAM response
             let consumeResp = dramResps.canPeek .&.
                               inflightQueue.canDeq
             -- Consume DRAM response
             when consumeResp do
               -- Accumulate responses
               dramResps.consume
               if loadCount.val .==. info.coalInfoBurstLen
                 then do
                   loadCount <== 0
                   state6 <== s_Respond6
                 else do
                   loadCount <== loadCount.val + 1

               sequence_
                 [ when newValid do
                     dynamicAssert (inv oldValid)
                       "Coalescing unit: loosing DRAM resp"
                     accum <==
                       MemResp {
                         memRespData = useSameBlock ? (d, sameAddrData)
                       , memRespDataTagBit = tMask .&&.
                           (useSameBlock ? (t, sameAddrTagBit))
                       , memRespIsFinal = info.coalInfoIsFinal
                       }
                 | (newValid, oldValid, accum, d, t, tMask) <-
                     zip6 activeAny
                          (toBitList dramRespsAccumValid.val)
                          dramRespsAccum
                          (V.toList sameBlockData)
                          (toBitList sameBlockTagBits)
                          (toBitList info.coalInfoTagBitMask) ]
               dramRespsAccumValid <== dramRespsAccumValid.val .|.
                                         fromBitList activeAny
               dramRespId <== info.coalInfoReqId

  -- Stage 6 (SRAM): Handle responses
  -- ================================

  -- Merge SRAM and DRAM responses
  let isFinalVec (id, v) = orList
        [ valid .&&. resp.memRespIsFinal
        | Option valid resp <- V.toList v ]

  finalResps <- makeGenericFairMergeTwo
                  (makePipelineQueue 1)
                  (const true) isFinalVec
                  (toStream dramRespQueue, sramResps)

  let perfStats =
        CoalUnitPerfStats {
          incLoadHit = incStoreBufferLoadHit.val
        , incLoadMiss = incStoreBufferLoadMiss.val
        , isCapMetaAccess = isCapMetaAccess.val
        }

  return (finalResps, toStream sramReqs, toStream dramReqQueue, perfStats)

-- Scalarised Vector Store Buffer (SVSB) Types
-- ===========================================

-- The direct-mapped store buffer holds scalarised vectors and is
-- intended to reduce the cost of stack spills.

-- | Number of beats per vector
type DRAMLogBeatsPerVec = SIMTLogLanes + 2 - DRAMBeatLogBytes

-- | Width of an address of a vector in DRAM
type DRAMVecAddrWidth = DRAMAddrWidth - DRAMLogBeatsPerVec

-- | Address of a vector in DRAM
type DRAMVecAddr = Bit DRAMVecAddrWidth

-- | Index into store buffer
type SVSBIndex = Bit SIMTSVStoreBufferLogSize

-- | Upper bits of a vec address for entries in the direct-mapped store buffer
type SVSBTag = Bit (DRAMVecAddrWidth - SIMTSVStoreBufferLogSize)

-- | Store buffer entry
data SVSBEntry =
  SBEntry {
    valid :: Bit 1
    -- ^ Is this entry valid?
  , tag :: SVSBTag
    -- ^ Upper bits of address
  , scalarVal :: ScalarVal 33
    -- ^ Scalarised vector
  } deriving (Generic, Bits)
