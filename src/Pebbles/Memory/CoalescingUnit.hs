-- Simple coalescing unit connecting SIMT lanes to DRAM and banked SRAMs

module Pebbles.Memory.CoalescingUnit 
  ( makeCoalescingUnit
  , BankInfo(..)
  ) where

-- SoC parameters
#include <Config.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.Option
import Blarney.SourceSink
import Blarney.Interconnect
import qualified Blarney.Vector as V

-- Pebbles imports
import Pebbles.Util.List
import Pebbles.Pipeline.Interface
import Pebbles.Memory.Interface
import Pebbles.Memory.Alignment
import Pebbles.Memory.DRAM.Interface

-- Haskell imports
import Data.List
import Data.Proxy
import Control.Monad (forM_)

-- Types
-- =====

-- | DRAM request ids from the coalescing unit are unused
type DRAMReqId = ()

-- | Info for inflight DRAM requests (internal to this module)
data CoalescingInfo t_id =
  CoalescingInfo {
    coalInfoUseSameBlock :: Bit 1
    -- ^ Use the SameBlock stategy?  (Otherwise, use SameAddress strategy)
  , coalInfoMask :: Bit SIMTLanes
    -- ^ Coalescing mask (lanes participating in coalesced access)
  , coalInfoReqIds :: V.Vec SIMTLanes t_id
    -- ^ Request id for each lane
  , coalInfoSameBlockMode :: Bit 2
    -- ^ Mode for SameBlock strategy
  , coalInfoAddr :: Bit DRAMBeatLogBytes
    -- ^ Lower bits of address
  , coalInfoBurstLen :: DRAMBurst
    -- ^ Burst length
  , coalInfoIsFinal :: Bit 1
    -- ^ Final request in transaction?
  } deriving (Generic, Bits)

-- | SRAM bank request info
data BankInfo t_id =
  BankInfo {
    bankReqId :: t_id
    -- ^ Request id
  , bankLaneId :: Bit SIMTLogLanes
    -- ^ Id of issuing lane
  , bankMcastId :: Option (Bit SIMTMcastIdSize)
    -- ^ Response should be multicast to multiple lanes
  }
  deriving (Generic, Interface, Bits)

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
makeCoalescingUnit :: Bits t_id =>
     (MemReq t_id -> Bit 1)
     -- ^ Predicate to determine if request is for SRAM (true) or DRAM (false)
  -> Stream (V.Vec SIMTLanes (Option (MemReq t_id)))
     -- ^ Stream of memory requests vectors
  -> Stream (DRAMResp DRAMReqId)
     -- ^ Responses from DRAM
  -> V.Vec SIMTLanes (Stream (MemResp (BankInfo t_id)))
     -- ^ Responses from SRAM, per lane/bank
  -> Module ( V.Vec SIMTLanes (Stream (MemResp t_id))
            , V.Vec SIMTLanes (Stream (MemReq (BankInfo t_id)))
            , Stream (DRAMReq DRAMReqId)
            )
     -- ^ Outputs: memory responses per lane, SRAM requests per
     -- lane/bank, and DRAM requests
makeCoalescingUnit isSRAMAccess memReqsStream dramResps sramRespsVec = do
  let sramResps = V.toList sramRespsVec

  -- Assumptions
  staticAssert (SIMTLanes == DRAMBeatHalfs)
    ("Coalescing Unit: number of SIMT lanes must equal " ++
     "number of half-words in DRAM beat")

  -- Trigger signals for each pipeline stage
  go1 :: Reg (Bit 1) <- makeDReg false
  go2 :: Reg (Bit 1) <- makeDReg false
  go3 :: Reg (Bit 1) <- makeDReg false
  go4 :: Reg (Bit 1) <- makeDReg false
  go5DRAM :: Reg (Bit 1) <- makeReg false
  go5SRAM :: Reg (Bit 1) <- makeReg false

  -- Requests for each pipeline stage
  memReqs1 :: [Reg (MemReq t_id)] <- replicateM SIMTLanes (makeReg dontCare)
  memReqs2 :: [Reg (MemReq t_id)] <- replicateM SIMTLanes (makeReg dontCare)
  memReqs3 :: [Reg (MemReq t_id)] <- replicateM SIMTLanes (makeReg dontCare)
  memReqs4 :: [Reg (MemReq t_id)] <- replicateM SIMTLanes (makeReg dontCare)
  memReqs5DRAM :: [Reg (MemReq t_id)] <-
    replicateM SIMTLanes (makeReg dontCare)
  memReqs5SRAM :: [Reg (MemReq t_id)] <-
    replicateM SIMTLanes (makeReg dontCare)

  -- Pending request mask for each pipeline stage
  pending1 :: Reg (Bit SIMTLanes) <- makeReg dontCare
  pending2 :: Reg (Bit SIMTLanes) <- makeReg dontCare
  pending3 :: Reg (Bit SIMTLanes) <- makeReg dontCare
  pending4 :: Reg (Bit SIMTLanes) <- makeReg dontCare

  -- Leader requests for each pipeline stage
  leaderReq3 :: Reg (MemReq t_id) <- makeReg dontCare
  leaderReq4 :: Reg (MemReq t_id) <- makeReg dontCare
  leaderReq5 :: Reg (MemReq t_id) <- makeReg dontCare

  -- Bit vector identifying the chosen leader
  leader2 :: Reg (Bit SIMTLanes) <- makeReg 0
  leader3 :: Reg (Bit SIMTLanes) <- makeReg 0
  leader4 :: Reg (Bit SIMTLanes) <- makeReg 0
  leader5SRAM :: Reg (Bit SIMTLanes) <- makeReg 0

  -- DRAM request queue
  dramReqQueue :: Queue (DRAMReq DRAMReqId) <- makePipelineQueue 1

  -- Inflight DRAM requests
  inflightQueue :: Queue (CoalescingInfo t_id) <-
    makeSizedQueue DRAMLogMaxInFlight

  -- DRAM response queues
  -- There's a lot of logic feeding these queues, so let's use a
  -- multi-level shift queue
  dramRespQueues :: [Queue (MemResp t_id)] <-
    replicateM SIMTLanes (makeShiftQueue 2)

  -- Stage 0: consume requests and feed pipeline
  -- ===========================================

  -- Pipeline feedback trigger from stage 3
  feedbackWire :: Wire (Bit 1) <- makeWire false

  -- Pipeline stall wire
  stallWire :: Wire (Bit 1) <- makeWire false

  -- Are we currently injecting a multi-flit transaction in the pipeline?
  isTransaction :: Reg (Bit 1) <- makeReg false

  always do
    -- Invariant: feedback and stall never occur together)
    dynamicAssert (inv (feedbackWire.val .&. stallWire.val))
      "Coalescing Unit: feedback and stall both high"

    -- Memory requests to consume
    let memReqs = V.toList (memReqsStream.peek)

    -- Is this the final flit of a multi-flit transaction?
    let isFinal = orList [ r.valid .&&. r.val.memReqIsFinal
                         | r <- memReqs ]

    -- A multi-flit transaction can only proceed if there is space in
    -- pipeline for two flits (transactions are currently limited
    -- to a max of two flits)
    let multiFlitOk = isFinal .||. go1.val.inv

    -- Inject requests from input queues when no stall/feedback in progress
    when (memReqsStream.canPeek) do
      when (stallWire.val.inv .&&.
              feedbackWire.val.inv .&&. multiFlitOk .||.
                isTransaction.val) do
        -- Consume next vector of requests
        memReqsStream.consume
        -- Inject into pipeline
        forM_ (zip memReqs memReqs1) \(req, reg) -> do
          reg <== req.val
        -- Initialise pending mask
        pending1 <== fromBitList [r.valid | r <- memReqs]
        -- Trigger pipeline
        go1 <== true
        -- Handle multi-flit transactions atomically
        isTransaction <== inv isFinal

    -- Preserve go signals on stall
    when (stallWire.val) do
      when (go1.val .&&. isTransaction.val.inv) do go1 <== true
      when (go2.val) do go2 <== true
      when (go3.val) do go3 <== true
      when (go4.val) do go4 <== true

  -- Stage 1: Pick a leader
  -- ======================

  always do
    when (go1.val .&&. (stallWire.val.inv .||. isTransaction.val)) do
      -- Select first pending request as leader
      leader2 <== pending1.val .&. (pending1.val.inv + 1)
      -- In a multi-flit transaction, there will be space in the pipeline
      dynamicAssert (isTransaction.val .==>. go2.val.inv)
        "Coalescing Unit (stage 1): pipeline overflow, mutli-flit transaction"
      -- Trigger stage 2
      go2 <== true
      zipWithM_ (<==) memReqs2 (map val memReqs1)
      pending2 <== pending1.val

  -- Stage 2: Select leader's request
  -- ================================

  always do
    when (go2.val .&. stallWire.val.inv) do
      -- There must exist at least one possible leader
      dynamicAssert (leader2.val .!=. 0)
        "Coalescing Unit (Stage 2): no leader found"
      -- Mux to select leader's request
      leaderReq3 <== select (zip (leader2.val.toBitList)
                                 (map val memReqs2))
      -- Trigger stage 3
      go3 <== true
      zipWithM_ (<==) memReqs3 (map val memReqs2)
      pending3 <== pending2.val
      leader3 <== leader2.val

  -- Stage 3: Evaluate coalescing strategies
  -- =======================================

  -- Outcome of stage 3
  sameBlockMode4 :: Reg (Bit 2) <- makeReg dontCare
  sameBlockMask4 :: Reg (Bit SIMTLanes) <- makeReg dontCare
  sameAddrMask4  :: Reg (Bit SIMTLanes) <- makeReg dontCare
  sramMask4      :: Reg (Bit SIMTLanes) <- makeReg dontCare
  isSRAMAccess4  :: Reg (Bit 1) <- makeReg dontCare

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
          | (req, valid) <- zip memReqs3 (pending3.val.toBitList) ]
    when (go3.val) do
      dynamicAssert sameOpAndAccessWidth
        "Coalescining unit: requests have different op or access width"

    -- Which requests can be satisfied by SameBlock strategy?
    -- ------------------------------------------------------

    -- There are three ways to satisfy the SameBlock Strategy: either
    -- the requests access a contiguous array of bytes (ByteMode=0),
    -- half words (HalfMode=1), or words (WordMode=2).  In ByteMode,
    -- the access width of each request must be a byte; in HalfMode,
    -- it must be a half word; in WordMode, it can be anything
    -- (as long as it is consistent).  This feature of WordMode
    -- allows efficient sub-word stack access, where stacks are
    -- interleaved at the word level.
    let sameBlockMatch req (laneId :: Bit SIMTLogLanes) =
            [ sameBlock .&&. byteMatch
            , sameBlock .&&. halfMatch
            , sameBlock .&&. wordMatch ]
          where
            a1 = req.memReqAddr
            a2 = leaderReq3.val.memReqAddr
            aw = leaderReq3.val.memReqAccessWidth
            sameBlock =
              slice @31 @(SIMTLogLanes+2) a1 .==.
                slice @31 @(SIMTLogLanes+2) a2
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
            | (r, i) <- zip (map val memReqs3) [0..] ]

    -- Take into account which requests are valid
    let [byteModeMask, halfModeMask, wordModeMask] =
          map (pending3.val .&.) sameBlockMasks

    -- Which requests can be satisfied by SameAddress strategy?
    -- --------------------------------------------------------

    -- Requests satisfied by SameAddress strategy
    let sameAddrMaskVal :: Bit SIMTLanes = fromBitList
          [ p .&&. r.memReqAddr .==. leaderReq3.val.memReqAddr
          | (p, r) <- zip (pending3.val.toBitList) (map val memReqs3) ]

    -- Requests destined for banked SRAMs
    let sramMask = 
          [ p .&. isSRAMAccess r
          | (p, r) <- zip (pending3.val.toBitList) (map val memReqs3) ]

    -- State update
    when (go3.val .&. stallWire.val.inv) do
      -- SameAddress strategy should at least allow the leader to progress
      dynamicAssert (sameAddrMaskVal .!=. 0)
        "Coalescing Unit: SameAddr strategy does not make progress!"
      -- Requests satisifed by SameAddress strategy
      sameAddrMask4 <== sameAddrMaskVal
      -- For SameBlock strategy, choose WordMode if it satisfies leader
      -- and at least one other request
      let useWordMode = (wordModeMask .&. leader3.val .!=. 0) .&.
                          (wordModeMask .&. leader3.val.inv .!=. 0)
      if useWordMode
        then do
          sameBlockMode4 <== 2
          sameBlockMask4 <== wordModeMask
        else do
          -- Otherwise, use access width to determine mode
          if leaderReq3.val.memReqAccessWidth.isHalfAccess
            then do
              sameBlockMode4 <== 1
              sameBlockMask4 <== halfModeMask
            else do
              sameBlockMode4 <== 0
              sameBlockMask4 <== byteModeMask
      -- Is leader accessing banked SRAMs?
      isSRAMAccess4 <== leaderReq3.val.isSRAMAccess
      sramMask4 <== fromBitList sramMask
      -- Trigger stage 4
      go4 <== true
      zipWithM_ (<==) memReqs4 (map val memReqs3)
      pending4 <== pending3.val
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

  -- Use SameAddress strategy on SRAM path
  useSameAddrSRAM :: Reg (Bit 1) <- makeReg dontCare

  -- Which lanes have requests for SRAM
  sramMask5 :: Reg (Bit SIMTLanes) <- makeReg dontCare

  -- Final request of DRAM transaction?
  isFinalDRAM :: Reg (Bit 1) <- makeReg dontCare

  always do
    -- Use SameBlock strategy if it satisfies leader's request and at
    -- least one other request.  Otherwise use SameAddr strategy,
    -- which will always satisfy at least one request.
    let useSameBlock =
          ((sameBlockMask4.val .&. leader3.val) .==. leader3.val) .&.
            ((sameBlockMask4.val .&. leader3.val.inv) .!=. 0)
    -- Requests participating in strategy
    let mask = isSRAMAccess4.val ? (sramMask4.val,
                 useSameBlock ? (sameBlockMask4.val, sameAddrMask4.val))
    -- Try to trigger next stage
    when (go4.val) do
      let busy = isSRAMAccess4.val ? (go5SRAM.val, go5DRAM.val)
      -- Stall if stage 5 is currently busy
      -- Stall if multi-flit transaction being injected into stage 1
      if busy .||. isTransaction.val
        then do
          -- If so, stall pipeline
          stallWire <== true
        else do
          -- Otherwise, setup and trigger next stage
          if isSRAMAccess4.val
            then do
              go5SRAM <== true
              sramMask5 <== sramMask4.val
              leader5SRAM <== leader4.val
              forM_ (zip memReqs4 memReqs5SRAM) \(r4, r5) -> do
                r5 <== r4.val
              -- Use same address strategy if all SRAM accesses are
              -- loads to the same address
              useSameAddrSRAM <== leaderReq4.val.memReqOp .==. memLoadOp .&&.
                sramMask4.val .==. sameAddrMask4.val
            else do
              go5DRAM <== true
              coalSameBlockStrategy <== useSameBlock
              coalSameBlockMode <== sameBlockMode4.val
              coalMask <== mask
              leaderReq5 <== leaderReq4.val
              isFinalDRAM <== leaderReq4.val.memReqIsFinal
              forM_ (zip memReqs4 memReqs5DRAM) \(r4, r5) -> do
                r5 <== r4.val
              -- Check that atomics are not in use
              dynamicAssert (leaderReq4.val.memReqOp .!=. memAtomicOp)
                "Atomics not yet supported on DRAM path"
              dynamicAssert (leaderReq4.val.memReqOp .!=. memLocalFenceOp)
                "Local fence not supported on DRAM path"
          -- Determine any remaining pending requests
          let remaining = pending4.val .&. inv mask
          -- If there are any, feed them back
          when (remaining .!=. 0) do
            go1 <== true
            feedbackWire <== true
            zipWithM_ (<==) memReqs1 (map val memReqs4)
            pending1 <== remaining

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
                sameBlockMode.isByteAccess --> (1, 0b0)
              , sameBlockMode.isHalfAccess --> (1, 0b0)
              , sameBlockMode.isWordAccess --> (2, 0b1)
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
            | [r1, r2, r3, r4] <- groupsOf 4 memReqs5DRAM]
    let sameBlockData16 :: V.Vec DRAMBeatHalfs (Bit 16) =
          V.fromList $ concat $
            [ [r1.val.memReqData.lower, r2.val.memReqData.upper]
            | [r1, r2] <- groupsOf 2 memReqs5DRAM ]
    let sameBlockData32 :: V.Vec DRAMBeatWords (Bit 32) =
          V.fromList $ selectHalf (storeCount.val.truncate)
            [r.val.memReqData | r <- memReqs5DRAM]
    let sameBlockData :: DRAMBeat =
          [pack sameBlockData8,
             pack sameBlockData16,
               pack sameBlockData32] ! sameBlockMode
    -- Tag bits for SameBlock strategy
    let sameBlockTagBits8 :: Bit DRAMBeatWords =
          fromBitList $ concat $ replicate 2 $
            [ andList $ map memReqDataTagBit $ map val rs
            | rs <- groupsOf 4 memReqs5DRAM]
    let sameBlockTagBits16 :: Bit DRAMBeatWords =
          fromBitList
            [ andList $ map memReqDataTagBit $ map val rs
            | rs <- groupsOf 2 memReqs5DRAM]
    let sameBlockTagBits32 :: Bit DRAMBeatWords =
          fromBitList $ selectHalf (storeCount.val.truncate)
            [r.val.memReqDataTagBit | r <- memReqs5DRAM]
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
            [en .&. inv useUpper | en <- mask.toBitList] ++
            [en .&. useUpper | en <- mask.toBitList]
    let sameBlockBE16 :: Bit DRAMBeatBytes =
          fromBitList $ concatMap (replicate 2) (mask.toBitList)
    let sameBlockBE32 :: Bit DRAMBeatBytes =
          fromBitList $ concatMap toBitList $
            selectHalf (storeCount.val.truncate)
              [ rep en .&.
                  genByteEnable
                    (r.val.memReqAccessWidth)
                    (r.val.memReqAddr)
              | (en, r) <- zip (mask.toBitList) memReqs5DRAM ]
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
          , dramReqAddr = dramAddr.truncate .&. addrMask.zeroExtend.inv
          , dramReqData = useSameBlock ? (sameBlockData, sameAddrData)
          , dramReqDataTagBits =
              useSameBlock ? (sameBlockTagBits, sameAddrTagBits)
          , dramReqByteEn = useSameBlock ? (sameBlockBE, sameAddrBE)
          , dramReqBurst = burstLen
          , dramReqIsFinal =
              isStore ? (isFinalStore .&&. isFinalDRAM.val, isFinalDRAM.val)
          }
    -- Try to issue DRAM request
    when (go5DRAM.val) do
      -- Check that we can make a DRAM request
      when (inflightQueue.notFull .&. dramReqQueue.notFull) do
        -- Issue DRAM request
        enq dramReqQueue dramReq
        -- Info needed to process response
        let info =
              CoalescingInfo {
                coalInfoUseSameBlock = useSameBlock
              , coalInfoMask = mask
              , coalInfoReqIds =
                  V.fromList [r.val.memReqId | r <- memReqs5DRAM]
              , coalInfoSameBlockMode = sameBlockMode
              , coalInfoAddr = leaderReq5.val.memReqAddr.truncate
              , coalInfoBurstLen = burstLen - 1
              , coalInfoIsFinal = leaderReq5.val.memReqIsFinal
              }
        -- Handle load & fence: insert info into inflight queue
        let hasResp = leaderReq5.val.memReqOp .==. memLoadOp
                        .||. leaderReq5.val.memReqOp .==. memGlobalFenceOp
        when hasResp do
          enq inflightQueue info
          go5DRAM <== false
        -- Handle store: increment burst count
        when (leaderReq5.val.memReqOp .==. memStoreOp) do
          if isFinalStore
            then do
              storeCount <== 0
              go5DRAM <== false
            else do
              storeCount <== newStoreCount

  -- Stage 5 (SRAM): Issue SRAM requests
  -- ===================================

  -- Wires indicating when SRAM requests have been consumed
  sramConsumeWires <- replicateM SIMTLanes (makeWire false)

  -- Multicast array for same-address coalescing of banked SRAMs
  -- (Banked SRAM responses are out-of-order)
  mcastArray :: RAM (Bit SIMTMcastIdSize) (Bit SIMTLanes) <- makeDualRAM
  mcastIdNext :: Reg (Bit SIMTMcastIdSize) <- makeReg 0
  mcastIdsInUse :: [Reg (Bit 1)] <- replicateM (2^SIMTMcastIdSize) (makeReg 0)

  -- Is the next multicast id available?
  -- (Buffer this signal as obtaining ids is not latency critical)
  let mcastIdAvailable =
        delay false (inv (map val mcastIdsInUse ! mcastIdNext.val))

  -- Request stream per SRAM bank
  let sramReqs = 
        [ Source {
            -- For SameAddress strategy, only the leader makes a request
            canPeek = go5SRAM.val .&&.
                        (useSameAddrSRAM.val ?
                           (isLeader .&&. mcastIdAvailable, valid))
          , peek = req {
              memReqId =
                BankInfo {
                  bankReqId = req.memReqId
                  -- When using the SameAddress strategy, arrange for
                  -- the response to come back via lane 0
                , bankLaneId = useSameAddrSRAM.val ? (0, fromInteger i)
                  -- For SameAddress strategy, mark a multicast response
                , bankMcastId =
                    Option (useSameAddrSRAM.val) (mcastIdNext.val)
                }
            }
          , consume = consumeWire <== true
          }
        | (req, valid, consumeWire, isLeader, i) <-
            zip5 (map val memReqs5SRAM)
                 (sramMask5.val.toBitList)
                 sramConsumeWires
                 (leader5SRAM.val.toBitList)
                 [0..]
        ]

  always do
    when (go5SRAM.val) do
      -- Bit vector of streams being consumed
      let sramConsumeVec :: Bit SIMTLanes = fromBitList
            (map val sramConsumeWires)
      -- Remove consumed requests from bit mask
      sramMask5 <== sramMask5.val .&. inv sramConsumeVec
      -- Have all requests been consumed?
      let done = useSameAddrSRAM.val ?
                   (sramConsumeVec .==. leader5SRAM.val,
                    sramConsumeVec .==. sramMask5.val)
      when (done) do
        go5SRAM <== false
        -- Record multicast response in array
        when (useSameAddrSRAM.val) do
          store mcastArray (mcastIdNext.val) (sramMask5.val)
          (mcastIdsInUse ! mcastIdNext.val) <== true
          mcastIdNext <== mcastIdNext.val + 1

  -- Stage 6 (DRAM): Handle responses
  -- ================================

  -- Count register for burst load
  loadCount :: Reg DRAMBurst <- makeReg 0

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
                sameBlockMode.isByteAccess --> 0
              , sameBlockMode.isHalfAccess --> 0
              , sameBlockMode.isWordAccess -->
                  fromInteger (i `div` DRAMBeatWords)
              ]
          | i <- [0..SIMTLanes-1] ]
    -- Which lanes may deliver a response under any strategy?
    let deliverAny = map (useSameBlock .<=.) deliverSameBlock
    -- Consider only those lanes participating in the strategy
    let activeAny = zipWith (.&.) deliverAny (toBitList mask)
    -- Are needed response queues ready?
    let respQueuesReady =
          andList [ active .<=. q.notFull
                  | (active, q) <- zip activeAny dramRespQueues
                  ]
    -- Determine items of data response
    let beatBytes :: V.Vec DRAMBeatBytes (Bit 8) = unpack (resp.dramRespData)
    let beatHalfs :: V.Vec DRAMBeatHalfs (Bit 16) = unpack (resp.dramRespData)
    let beatWords :: V.Vec DRAMBeatWords (Bit 32) = unpack (resp.dramRespData)
    -- Response data for SameAddress strategy
    let sameAddrWordIndex :: Bit (DRAMBeatLogBytes-2) =
          info.coalInfoAddr.upper
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
                resp.dramRespDataTagBits.toBitList
    let sameBlockHalfTagBits :: Bit SIMTLanes =
          fromBitList $ concat
            [[b, b] | b <- resp.dramRespDataTagBits.toBitList]
    let sameBlockTagBits :: Bit SIMTLanes =
          [ sameBlockByteTagBits
          , sameBlockHalfTagBits
          , resp.dramRespDataTagBits # resp.dramRespDataTagBits
          ] ! sameBlockMode
    -- Condition for consuming DRAM response
    let consumeResp = dramResps.canPeek .&.
                      inflightQueue.canDeq .&.
                      respQueuesReady
    -- Consume DRAM response
    when consumeResp do
      dramResps.consume
      if loadCount.val .==. info.coalInfoBurstLen
        then do
          inflightQueue.deq
          loadCount <== 0
        else do
          loadCount <== loadCount.val + 1

      -- Response info for each SIMT lane
      let respInfo = zip5 dramRespQueues activeAny
                          (V.toList (info.coalInfoReqIds))
                          (V.toList sameBlockData)
                          (toBitList sameBlockTagBits)

      -- For each SIMT lane
      forM_ respInfo \(respQueue, active, id, d, t) -> do
        when active do
          enq respQueue
            MemResp {
              memRespId = id
            , memRespData = useSameBlock ? (d, sameAddrData)
            , memRespDataTagBit = useSameBlock ? (t, sameAddrTagBit)
            , memRespIsFinal = info.coalInfoIsFinal
            }

  -- Stage 6 (SRAM): Handle responses
  -- ================================

  -- SRAM response queues
  sramRespQueues :: [Queue (MemResp t_id)] <-
    replicateM SIMTLanes (makeShiftQueue 1)

  -- SRAM response state
  -- State 0: process non-multicast response
  -- State 1: process multicast response
  sramRespState :: Reg (Bit 1) <- makeReg 0

  -- Multicast SRAM response register
  mcastResp <- makeReg dontCare

  -- Helper to drop bank info from SRAM response id
  let sramUntag resp = resp { memRespId = resp.memRespId.bankReqId }

  always do
    when (sramRespState.val .==. 0) do
      -- Look for multicast responses (which always return via lane 0)
      -- that need to be delivered to multiple lanes
      let respStream0 = sramResps.head
      let resp0 = respStream0.peek
      if respStream0.canPeek .&&. resp0.memRespId.bankMcastId.valid
        then do
          let allReady = andList [q.notFull | q <- sramRespQueues]
          when allReady do
            load mcastArray (resp0.memRespId.bankMcastId.val)
            respStream0.consume
            mcastResp <== resp0
            sramRespState <== 1
        else do
          -- Handle non-multicast responses
          sequence_ [ when (s.canPeek .&&. q.notFull) do
                        enq q (s.peek.sramUntag)
                        s.consume
                    | (s, q) <- zip sramResps sramRespQueues ]

    -- Perform multicast
    when (sramRespState.val .==. 1) do
      (mcastIdsInUse ! mcastResp.val.memRespId.bankMcastId.val) <== false
      sequence_ [ when cond do enq q (mcastResp.val.sramUntag)
                | (cond, q) <- zip (mcastArray.out.toBitList) sramRespQueues ]
      sramRespState <== 0

  -- Merge SRAM and DRAM responses
  finalResps <- sequence
    [ makeGenericFairMergeTwo (makePipelineQueue 1) (const true)
       memRespIsFinal (toStream q0, toStream q1)
    | (q0, q1) <- zip dramRespQueues sramRespQueues ]

  return (V.fromList finalResps, V.fromList sramReqs, toStream dramReqQueue)
