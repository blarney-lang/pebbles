-- Simple coalescing unit connecting SIMT lanes to DRAM

module Pebbles.Memory.CoalescingUnit 
  ( makeCoalescingUnit
  ) where

-- SoC parameters
#include <SoC.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.SourceSink
import qualified Blarney.Vector as V

-- Pebbles imports
import Pebbles.Util.List
import Pebbles.Memory.Interface
import Pebbles.Memory.Alignment
import Pebbles.SoC.DRAM.Interface

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
    -- | Use the SameBlock stategy?  (Otherwise, use SameAddress strategy)
    coalInfoUseSameBlock :: Bit 1
    -- | Coalescing mask (lanes participating in coalesced access)
  , coalInfoMask :: Bit SIMTLanes
    -- | Request id for each lane
  , coalInfoReqIds :: V.Vec SIMTLanes t_id
    -- | Is unsigned load?
  , coalInfoIsUnsigned :: Bit 1
    -- | Access width
  , coalInfoAccessWidth :: AccessWidth
    -- | Lower bits of address
  , coalInfoAddr :: Bit DRAMBeatLogBytes
    -- | Burst length
  , coalInfoBurstLen :: DRAMBurst
  } deriving (Generic, Bits)

-- Implementation
-- ==============

-- | The coalescing unit takes memory requests from multiple SIMT
-- lanes and coalesces them where possible into a single DRAM request
-- using two strategies: (1) SameBlock: multiple accesses to the same
-- DRAM block, where the lower bits of the address equal the SIMT
-- lane id, are satisfied by a single DRAM burst; (2) SameAddress:
-- mutliple accesses to the same address are satisifed by a single
-- DRAM access. The second strategy (which always makes progress)
-- is used if the first fails.  We employ a seven stage pipeline:
--
--   0. Consume requests and feed pipeline
--   1. Pick one SIMT lane as a leader 
--   2. Determine the leader's request with a mux
--   3. Evaluate coalescing strategies
--   4. Choose coalescing strategy and feed back unsatisifed reqs to stage 1
--   5. Issue DRAM requests
--   6. Consume DRAM responses and issue load responses
--
-- Notes:
--   * The number of SIMT lanes is assumed to be equal to the
--     number of half-words in a DRAM Beat.
--   * We assume that DRAM responses come back in order.
--   * Backpressure on memory responses currently propagates to
--     DRAM responses, which could cause blocking on the DRAM bus.
makeCoalescingUnit :: Bits t_id =>
     -- | Inputs: responses from DRAM
     Stream (DRAMResp DRAMReqId)
     -- | Outputs: MemUnit per lane and a stream of DRAM requests
  -> Module ([MemUnit t_id], Stream (DRAMReq DRAMReqId))
makeCoalescingUnit dramResps = do
  -- Assumptions
  staticAssert (SIMTLanes == DRAMBeatHalfs)
    ("Coalescing Unit: number of SIMT lanes must equal " ++
     "number of half-words in DRAM beat")

  -- Input memory request queues
  memReqQueues :: [Queue (MemReq t_id)] <-
    replicateM SIMTLanes (makeShiftQueue 1)

  -- Trigger signals for each pipeline stage
  go1 :: Reg (Bit 1) <- makeDReg false
  go2 :: Reg (Bit 1) <- makeDReg false
  go3 :: Reg (Bit 1) <- makeDReg false
  go4 :: Reg (Bit 1) <- makeDReg false
  go5 :: Reg (Bit 1) <- makeReg false

  -- Requests for each pipeline stage
  memReqs1 :: [Reg (MemReq t_id)] <- replicateM SIMTLanes (makeReg dontCare)
  memReqs2 :: [Reg (MemReq t_id)] <- replicateM SIMTLanes (makeReg dontCare)
  memReqs3 :: [Reg (MemReq t_id)] <- replicateM SIMTLanes (makeReg dontCare)
  memReqs4 :: [Reg (MemReq t_id)] <- replicateM SIMTLanes (makeReg dontCare)
  memReqs5 :: [Reg (MemReq t_id)] <- replicateM SIMTLanes (makeReg dontCare)

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

  -- Choice of strategy
  coalSameBlockStrategy4 :: Reg (Bit 1) <- makeReg dontCare
  coalSameBlockStrategy5 :: Reg (Bit 1) <- makeReg dontCare

  -- Which lanes are participating in the strategy
  coalMask4 :: Reg (Bit SIMTLanes) <- makeReg dontCare
  coalMask5 :: Reg (Bit SIMTLanes) <- makeReg dontCare

  -- DRAM request queue
  dramReqQueue :: Queue (DRAMReq DRAMReqId) <- makePipelineQueue 1

  -- Inflight requests
  inflightQueue :: Queue (CoalescingInfo t_id) <-
    makeSizedQueue DRAMLogMaxInFlight

  -- Response queues
  respQueues :: [Queue (MemResp t_id)] <-
    replicateM SIMTLanes (makeShiftQueue 2)

  -- Stage 0: consume requests and feed pipeline
  -- ===========================================

  -- Pipeline feedback trigger from stage 3
  feedbackWire :: Wire (Bit 1) <- makeWire false

  -- Pipeline stall wire
  stallWire :: Wire (Bit 1) <- makeWire false

  always do
    -- Invariant: feedback and stall never occur together)
    dynamicAssert (inv (feedbackWire.val .&. stallWire.val))
      "Coalescing Unit: feedback and stall both high"

    -- Inject requests from input queues when no stall/feedback in progress
    when (stallWire.val.inv .&. feedbackWire.val.inv) do
      -- Consume requests and inject into pipeline
      forM_ (zip memReqQueues memReqs1) \(q, r) -> do
        when (q.canDeq) do
          q.deq
        r <== q.first
      -- Initialise pending mask
      pending1 <== fromBitList
        [ q.canDeq .&. (q.first.memReqOp .!=. memNullOp)
        | q <- memReqQueues ]
      -- Trigger pipeline
      go1 <== orList [q.canDeq | q <- memReqQueues]

    -- Preserve go signals on stall
    when (stallWire.val) do
      go1 <== go1.val
      go2 <== go2.val
      go3 <== go3.val
      go4 <== go4.val

  -- Stage 1: Pick a leader
  -- ======================

  always do
    when (go1.val .&. stallWire.val.inv) do
      -- Select first pending request as leader
      leader2 <== pending1.val .&. (pending1.val.inv + 1)
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

  always do
    -- Which requests can be satisfied by SameBlock strategy?
    -- ------------------------------------------------------

    -- Check if the lane alignment requirement is met for SameBlock
    let checkLane req (laneId :: Bit SIMTLogLanes) =
          select
            [ aw.isByteAccess --> slice @(SIMTLogLanes-1) @0 a .==. laneId
            , aw.isHalfAccess --> slice @SIMTLogLanes @1 a .==. laneId
            , aw.isWordAccess --> slice @(SIMTLogLanes+1) @2 a .==. laneId
            ]
          where
            a = req.memReqAddr
            aw = leaderReq3.val.memReqAccessWidth

    -- Check if the same block requirement is met for SameBlock
    let checkBlock req = sameBlockCommon .&.
          select
            [ aw .==. 0 --> slice @(SIMTLogLanes+1) @SIMTLogLanes a1 .==.
                            slice @(SIMTLogLanes+1) @SIMTLogLanes a2
            , aw .==. 1 --> at @(SIMTLogLanes+1) a1 .==.
                            at @(SIMTLogLanes+1) a2
            , aw .==. 2 --> true
            ]
          where
            a1 = req.memReqAddr
            a2 = leaderReq3.val.memReqAddr
            aw = leaderReq3.val.memReqAccessWidth
            sameBlockCommon =
              slice @31 @(SIMTLogLanes+2) a1 .==.
              slice @31 @(SIMTLogLanes+2) a2
        
    -- Requests satisfied by SameBlock strategy
    let sameBlockMask :: Bit SIMTLanes = fromBitList
          [ p .&. (r.memReqOp .==. leaderReq3.val.memReqOp)
              .&. (r.memReqAccessWidth .==. leaderReq3.val.memReqAccessWidth)
              .&. checkLane r (fromInteger i) .&. checkBlock r
          | (p, r, i) <- zip3 (pending3.val.toBitList)
                              (map val memReqs3)
                              [0..] ]

    -- Which requests can be satisfied by SameAddress strategy?
    -- --------------------------------------------------------

    -- Requests satisfied by SameAddress strategy
    let sameAddrMask :: Bit SIMTLanes = fromBitList
          [ p .&. (r.memReqOp .==. leaderReq3.val.memReqOp)
              .&. (r.memReqAddr .==. leaderReq3.val.memReqAddr)
              .&. (r.memReqAccessWidth .==. leaderReq3.val.memReqAccessWidth)
          | (p, r) <- zip (pending3.val.toBitList) (map val memReqs3) ]

    -- Feed back unsatisfied requests
    -- ------------------------------

    -- Choose strategy
    -- Use SameBlock strategy if it satisfies leader's request and at
    -- least one other requets thread.  Otherwise use SameAddr
    -- strategy, which will always satisfy at least one request.
    let useSameBlock =
          ((sameBlockMask .&. leader3.val) .==. leader3.val) .&.
            ((sameBlockMask .&. leader3.val.inv) .!=. 0)

    -- Requests participating in strategy
    let mask = useSameBlock ? (sameBlockMask, sameAddrMask)

    when (go3.val .&. stallWire.val.inv) do
      -- SameAddr should at least allow the leader to make progress
      dynamicAssert (sameAddrMask .!=. 0)
        "Coalescing Unit: SameAddr strategy does not make progress!"
      -- Trigger next stage
      go4 <== true
      coalSameBlockStrategy4 <== useSameBlock
      coalMask4 <== mask
      zipWithM_ (<==) memReqs4 (map val memReqs3)
      pending4 <== pending3.val
      leaderReq4 <== leaderReq3.val
      leader4 <== leader3.val

  -- Stage 4: Choose coalescing strategy
  -- ===================================

  always do
    -- Try to trigger next stage
    when (go4.val) do
      -- Check if stage 4 is currently busy
      if go5.val
        then do
          -- If so, stall pipeline
          stallWire <== true
        else do
          -- Otherwise, setup and trigger next stage
          go5 <== true
          leaderReq5 <==
            (leaderReq4.val) {
              memReqData =
                -- Align data field of leader request
                writeAlign (leaderReq4.val.memReqAccessWidth)
                           (leaderReq4.val.memReqData)
            }
          zipWithM_ (<==) memReqs5 (map val memReqs4)
          coalSameBlockStrategy5 <== coalSameBlockStrategy4.val
          coalMask5 <== coalMask4.val
          -- Determine any remaining pending requests
          let remaining = pending4.val .&. inv (coalMask4.val)
          -- If there are any, feed them back
          when (remaining .!=. 0) do
            go1 <== true
            feedbackWire <== true
            zipWithM_ (<==) memReqs1 (map val memReqs4)
            pending1 <== remaining

  -- Stage 5: Issue DRAM requests
  -- ============================

  -- Count register for burst store
  storeCount :: Reg DRAMBurst <- makeReg 0

  always do
    -- Shorthands for chosen strategy
    let useSameBlock = coalSameBlockStrategy5.val
    let mask = coalMask5.val
    -- Shorthand for access width
    let aw = leaderReq5.val.memReqAccessWidth
    -- Determine burst length and address mask (to align the burst)
    let (burstLen, addrMask) :: (DRAMBurst, Bit 1) =
          if useSameBlock
            then
              select [
                aw.isByteAccess --> (1, 0b0)
              , aw.isHalfAccess --> (1, 0b0)
              , aw.isWordAccess --> (2, 0b1)
              ]
            else (1, 0b0)
    -- The DRAM address is derived from the top bits of the memory address
    let dramAddr = slice @31 @DRAMBeatLogBytes (leaderReq5.val.memReqAddr)
    -- DRAM data field for SameBlock strategy
    let sameBlockData8 :: V.Vec DRAMBeatBytes (Bit 8) =
          V.fromList $ concat $ replicate 2
            [r.val.memReqData.truncate | r <- memReqs5]
    let sameBlockData16 :: V.Vec DRAMBeatHalfs (Bit 16) =
          V.fromList [r.val.memReqData.truncate | r <- memReqs5]
    let sameBlockData32 :: V.Vec DRAMBeatWords (Bit 32) =
          V.fromList $ selectHalf (storeCount.val.truncate)
            [r.val.memReqData | r <- memReqs5]
    let sameBlockData :: DRAMBeat =
          [pack sameBlockData8,
             pack sameBlockData16,
               pack sameBlockData32] ! aw
    -- DRAM data field for SameAddress strategy
    let sameAddrDataVec :: V.Vec DRAMBeatWords (Bit 32) =
          V.replicate (leaderReq5.val.memReqData)
    let sameAddrData :: DRAMBeat = pack sameAddrDataVec
    -- DRAM byte enable field for SameBlock strategy
    let useUpper = at @(DRAMBeatLogBytes-1) (leaderReq5.val.memReqAddr)
    let sameBlockBE8 :: Bit DRAMBeatBytes =
          fromBitList $
            [en .&. aw.isByteAccess .&. useUpper | en <- mask.toBitList] ++
            [en .&. aw.isByteAccess .&. inv useUpper | en <- mask.toBitList]
    let sameBlockBE16 :: Bit DRAMBeatBytes =
          fromBitList $ concatMap (replicate 2)
            [en .&. aw.isHalfAccess | en <- mask.toBitList]
    let sameBlockBE32 :: Bit DRAMBeatBytes =
          fromBitList $ concatMap (replicate 4) $
            selectHalf (storeCount.val.truncate)
              [en .&. aw.isWordAccess | en <- mask.toBitList]
    let sameBlockBE = orList [sameBlockBE8, sameBlockBE16, sameBlockBE32]
    -- DRAM byte enable field for SameAddress strategy
    let leaderBE = genByteEnable (leaderReq5.val.memReqAccessWidth)
                    (leaderReq5.val.memReqAddr)
    let subWord = slice @DRAMBeatLogBytes @2 (leaderReq5.val.memReqAddr)
    let sameAddrBEVec :: V.Vec DRAMBeatWords (Bit 4) = 
          V.fromList [ subWord .==. fromInteger i ? (leaderBE, 0)
                     | i <- [0..DRAMBeatWords-1] ]
    let sameAddrBE :: Bit DRAMBeatBytes = pack sameAddrBEVec
    -- Formulate DRAM request
    let dramReq =
          DRAMReq {
            dramReqId = ()
          , dramReqIsStore = leaderReq5.val.memReqOp .==. memStoreOp
          , dramReqAddr = dramAddr.truncate .&. addrMask.zeroExtend.inv
          , dramReqData = useSameBlock ? (sameBlockData, sameAddrData)
          , dramReqByteEn = useSameBlock ? (sameBlockBE, sameAddrBE)
          , dramReqBurst = burstLen
          }
    -- Try to issue DRAM request
    when (go5.val) do
      -- Check that we can make a DRAM request
      when (inflightQueue.notFull .&. dramReqQueue.notFull) do
        -- Issue DRAM request
        enq dramReqQueue dramReq
        -- Info needed to process response
        let info =
              CoalescingInfo {
                coalInfoUseSameBlock = useSameBlock
              , coalInfoMask = mask
              , coalInfoReqIds = V.fromList [r.val.memReqId | r <- memReqs5]
              , coalInfoIsUnsigned = leaderReq5.val.memReqIsUnsigned
              , coalInfoAccessWidth = leaderReq5.val.memReqAccessWidth
              , coalInfoAddr = leaderReq5.val.memReqAddr.truncate
              , coalInfoBurstLen = burstLen - 1
              }
        -- Handle load: insert info into inflight queue
        when (leaderReq5.val.memReqOp .==. memLoadOp) do
          enq inflightQueue info
          go5 <== false
        -- Handle store: increment burst count
        when (leaderReq5.val.memReqOp .==. memStoreOp) do
          let newStoreCount = storeCount.val + 1
          if newStoreCount .==. burstLen
            then do
              storeCount <== newStoreCount
            else do
              storeCount <== 0
              go5 <== false

  -- Stage 6: Handle DRAM responses
  -- ==============================

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
    let aw = info.coalInfoAccessWidth
    let isUnsigned = info.coalInfoIsUnsigned
    -- Which lanes may deliver a response under SameBlock strategy?
    let deliverSameBlock =
          [ loadCount.val .==. 
              select [
                aw.isByteAccess --> 0
              , aw.isHalfAccess --> 0
              , aw.isWordAccess --> fromInteger (i `div` DRAMBeatWords)
              ]
          | i <- [0..SIMTLanes-1] ]
    -- Which lanes may deliver a response under any strategy?
    let deliverAny = map (useSameBlock .<=.) deliverSameBlock
    -- Consider only those lanes participating in the strategy
    let activeAny = zipWith (.&.) deliverAny (toBitList mask)
    -- Are needed response queues ready?
    let respQueuesReady =
          andList [ active .<=. q.notFull
                  | (active, q) <- zip activeAny respQueues
                  ]
    -- Determine items of data response
    let beatBytes :: V.Vec DRAMBeatBytes (Bit 8) = unpack (resp.dramRespData)
    let beatHalfs :: V.Vec DRAMBeatHalfs (Bit 16) = unpack (resp.dramRespData)
    let beatWords :: V.Vec DRAMBeatWords (Bit 32) = unpack (resp.dramRespData)
    -- Response data for SameAddress strategy
    let sameAddrWordIndex :: Bit (DRAMBeatLogBytes-2) =
          info.coalInfoAddr.upper
    let sameAddrWord = beatWords ! sameAddrWordIndex
    let sameAddrData =
          loadMux sameAddrWord
            (info.coalInfoAddr.truncate)
            (info.coalInfoAccessWidth)
            (info.coalInfoIsUnsigned)
    -- Response data for SameBlock strategy
    let sameBlockBytes :: V.Vec SIMTLanes (Bit 32) =
          V.fromList $
            map (\x -> isUnsigned ? (zeroExtend x, signExtend x)) $
              selectHalf (at @(DRAMBeatLogBytes-1) (info.coalInfoAddr)) $
                V.toList beatBytes
    let sameBlockHalfs :: V.Vec SIMTLanes (Bit 32) =
          V.map (\x -> isUnsigned ? (zeroExtend x, signExtend x)) beatHalfs
    let sameBlockData :: V.Vec SIMTLanes (Bit 32) =
          [ sameBlockBytes
          , sameBlockHalfs
          , beatWords `V.append` beatWords
          ] ! aw
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
      let respInfo = zip4 respQueues activeAny
                          (V.toList (info.coalInfoReqIds))
                          (V.toList sameBlockData)

      -- For each SIMT lane
      forM_ respInfo \(respQueue, active, id, d) -> do
        when active do
          let respData = useSameBlock ? (d, sameAddrData)
          enq respQueue (MemResp id respData)

  -- Memory interfaces
  let memUnits =
        [ MemUnit {
            memReqs = toSink reqQueue
          , memResps = toStream respQueue
          }
        | (reqQueue, respQueue) <- zip memReqQueues respQueues
        ]

  return (memUnits, toStream dramReqQueue)
