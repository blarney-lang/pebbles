module Pebbles.Memory.BankedSRAMs
  ( makeBankedSRAMs
  ) where

-- SoC parameters
#include <Config.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.Option
import Blarney.Interconnect
import Blarney.Vector (Vec, toList, fromList)

-- Pebbles imports
import Pebbles.Util.List
import Pebbles.Util.Counter
import Pebbles.Memory.Interface
import Pebbles.Memory.Alignment

-- Haskell imports
import Data.List
import Control.Applicative

-- The number of SRAM banks may be less than the number of lanes,
-- which is resolved by serialisation.  During serialisation, we use a
-- group id to remember which subgroup of the lane requests a bank
-- request comes from.
type GroupId = Bit (SIMTLogLanes - SIMTLogSRAMBanks)

-- | Create an array of indepdendent SRAM banks that are accessible as
-- a multi-port shared memory.  We use a crossbar to route requests to
-- (and responses from) the appropriate bank.  If the input streams
-- always provide requests from the same warp (in any given clock
-- cycle), then so too will the output streams.  Responses are
-- returned in order.  See note [Capability atomicity in banked SRAMs].
makeBankedSRAMs :: forall t_id. (Bits t_id, Interface t_id) =>
     Stream (Vec SIMTLanes (Option (MemReq t_id)), Option (MemReq t_id))
     -- ^ Stream of memory requests per lane, and leader request which
     -- is only valid when all requests are accessing the same address
  -> Module (Stream (Vec SIMTLanes (Option (MemResp t_id))))
     -- ^ Stream of memory responses per lane
makeBankedSRAMs inputs = do

  -- Assumptions
  staticAssert (SIMTLogSRAMBanks <= SIMTLogLanes)
    "Banked SRAMs: number of banks must be <= number of lanes"

  -- SRAM banks implemented by block RAMs
  -- (Most significant byte holds tag bit)
  sramBanks :: [RAMBE SIMTLogWordsPerSRAMBank 5] <-
    replicateM SIMTSRAMBanks makeDualRAMForwardBE

  -- Requests for each pipeline stage
  reqs1 :: [Reg (MemReq t_id, GroupId)] <-
    replicateM SIMTSRAMBanks (makeReg dontCare)
  reqs2 :: [Reg (MemReq t_id)] <-
    replicateM SIMTSRAMBanks (makeReg dontCare)
  reqs3 :: [Reg (MemReq t_id)] <-
    replicateM SIMTSRAMBanks (makeReg dontCare)

  -- Responses
  resps4 :: [Reg (MemResp t_id)] <-
    replicateM SIMTSRAMBanks (makeReg dontCare)
  resps5 :: [Reg (MemResp t_id)] <-
    replicateM SIMTSRAMBanks (makeReg dontCare)

  -- Final response queue
  respQueue :: Queue (Vec SIMTLanes (Option (MemResp t_id))) <-
    makeShiftQueue 1

  -- Active masks for each pipeline stage
  active1 :: Reg (Bit SIMTSRAMBanks) <- makeReg dontCare
  active2 :: Reg (Bit SIMTSRAMBanks) <- makeReg dontCare
  active3 :: Reg (Bit SIMTSRAMBanks) <- makeReg dontCare
  active4 :: Reg (Bit SIMTSRAMBanks) <- makeReg dontCare
  active5 :: Reg (Bit SIMTSRAMBanks) <- makeReg dontCare

  -- Original active mask (pre crossbar) for each stage
  origActive1 :: Reg (Bit SIMTSRAMBanks) <- makeReg dontCare
  origActive2 :: Reg (Bit SIMTSRAMBanks) <- makeReg dontCare
  origActive3 :: Reg (Bit SIMTSRAMBanks) <- makeReg dontCare

  -- Original bank target per request for each stage
  origBankTargets2 :: [Reg (Bit SIMTLogSRAMBanks)] <-
    replicateM SIMTSRAMBanks (makeReg dontCare)
  origBankTargets3 :: [Reg (Bit SIMTLogSRAMBanks)] <-
    replicateM SIMTSRAMBanks (makeReg dontCare)
  origBankTargets4 :: [Reg (Bit SIMTLogSRAMBanks)] <-
    replicateM SIMTSRAMBanks (makeReg dontCare)

  -- The subgroup of the request streams that each bank request comes from
  groupIds2 :: [Reg GroupId] <- replicateM SIMTSRAMBanks (makeReg dontCare)
  groupIds3 :: [Reg GroupId] <- replicateM SIMTSRAMBanks (makeReg dontCare)
  groupIds4 :: [Reg GroupId] <- replicateM SIMTSRAMBanks (makeReg dontCare)
  groupIds5 :: [Reg GroupId] <- replicateM SIMTSRAMBanks (makeReg dontCare)

  -- Multicast mask per pipeline stage; valid only for same address strategy
  mcastMask1 :: Reg (Option (Bit SIMTLanes)) <- makeReg none
  mcastMask2 :: Reg (Option (Bit SIMTLanes)) <- makeReg none
  mcastMask3 :: Reg (Option (Bit SIMTLanes)) <- makeReg none
  mcastMask4 :: Reg (Option (Bit SIMTLanes)) <- makeReg none
  mcastMask5 :: Reg (Option (Bit SIMTLanes)) <- makeReg none

  -- This bit marks that serialisation of the current request set is complete
  last1 :: Reg (Bit 1) <- makeReg dontCare
  last2 :: Reg (Bit 1) <- makeReg dontCare
  last3 :: Reg (Bit 1) <- makeReg dontCare
  last4 :: Reg (Bit 1) <- makeReg dontCare
  last5 :: Reg (Bit 1) <- makeReg dontCare

  -- Trigger for each pipeline stage
  go1 :: Reg (Bit 1) <- makeDReg false
  go2 :: Reg (Bit 1) <- makeDReg false
  go3 :: Reg (Bit 1) <- makeDReg false
  go4 :: Reg (Bit 1) <- makeDReg false
  go5 :: Reg (Bit 1) <- makeDReg false

  -- For triggering a pipeline stall
  stallWire :: Wire (Bit 1) <- makeWire false

  -- Pipeline stall
  -- ==============

  always do
    -- On a stall, retry each pipeline stage on next cycle
    when (stallWire.val) do
      go1 <== go1.val
      go2 <== go2.val
      go3 <== go3.val
      go4 <== go4.val
      go5 <== go5.val

  -- Prepare requests
  -- ================

  -- Bit mask of lanes that have been consumed
  consumedMask :: Reg (Bit SIMTLanes) <- makeReg 0

  -- Bit mask of lanes that are consuming on the current cycle
  consumeWires :: [Wire (Bit 1)] <- replicateM SIMTLanes (makeWire false)

  -- This wire is pulsed to indicate the requests from all active
  -- lanes have been inserted into the pipeline
  consumedAll :: Wire (Bit 1) <- makeWire false

  -- Convert stream of vectors to vector of streams
  -- If all requests accessing same address, then issue only via lane 0
  let reqStreams = 
        [ Source {
            canPeek =
              andList [
                inputs.canPeek
              , inv consumed
              , if i == 0
                  then (snd inputs.peek).valid .||. req.valid
                  else inv (snd inputs.peek).valid .&&. req.valid
              ]
          , peek = if i == 0
                      then (snd inputs.peek).valid ?
                             ((snd inputs.peek).val, req.val)
                      else req.val
          , consume = consumeWire <== true
          }
        | (req, consumeWire, consumed, i) <-
            zip4 (toList (fst inputs.peek))
                 consumeWires
                 (toBitList consumedMask.val)
                 [0..]
        ]

  -- Ensure that all requests have been injected into the pipeline
  -- before consuming the request vector
  always do
    when (inputs.canPeek) do
      -- Bit mask of available requests
      let avail = if (snd inputs.peek).valid
                    then 1 {- Lane 0 -}
                    else fromBitList $ map (.valid) $ toList (fst inputs.peek)
      -- Bit mask of requests being consumed on current cycle
      let consumeNow = fromBitList $ map (.val) consumeWires
      -- Have all requests been consumed?
      -- Uncomment the disjuntion for higher throughput but lower Fmax
      if avail .==. consumedMask.val {- .|. consumeNow -}
        then do
          inputs.consume
          consumedMask <== 0
        else do
          consumedMask <== consumedMask.val .|. consumeNow
      -- Observe cycle when all requests have been consumed
      when (avail .==. consumedMask.val .|. consumeNow) do
        consumedAll <== true
  
  -- Stage 0: Consume inputs and compute crossbar indices
  -- ====================================================

  -- Indices for croassbar switch
  indices1 :: [Reg (Bit SIMTLogSRAMBanks)] <-
    replicateM SIMTSRAMBanks (makeReg dontCare)

  -- Merge streams, giving one stream per bank
  let reqsPerBank = map mergeTree
        [ [ s { peek = (s.peek, fromInteger i :: GroupId) }
          | (s, i) <- zip ss [0..] ]
        | ss <- transpose (groupsOf SIMTSRAMBanks reqStreams) ]

  -- What's the destination bank for the given request?
  let bankForReq req = slice @(SIMTLogSRAMBanks+1) @2 (req.memReqAddr)

  -- Get bit mask for requests destined for given bank
  let genBankMask :: Bit SIMTLogSRAMBanks -> Bit SIMTSRAMBanks
      genBankMask b = firstHot $ fromBitList
        [s.canPeek .&&. b .==. bankForReq (fst s.peek) | s <- reqsPerBank]

  always do
    when (inv stallWire.val) do
      -- Determine which request maps to each bank
      let masks = [genBankMask b | b <- map fromInteger [0..SIMTSRAMBanks-1]]
      zipWithM_ (<==) indices1 (map binaryEncode masks)
      -- Chosen banks to consume from
      let chosenMask = orList masks
      -- Consume requests
      sequence_
        [ when cond do s.consume
        | (s, cond) <- zip reqsPerBank (toBitList chosenMask) ]
      -- Trigger next stage
      origActive1 <== chosenMask
      active1 <== fromBitList [m .!=. 0 | m <- masks]
      zipWithM_ (<==) reqs1 (map peek reqsPerBank)
      mcastMask1 <==
        Option (snd inputs.peek).valid
               (fromBitList $ map (.valid) $ toList $ fst inputs.peek)
      last1 <== consumedAll.val
      when (chosenMask .!=. 0) do
        go1 <== true

  -- Stage 1: Crossbar switch (for requests)
  -- =======================================

  always do
    when (inv stallWire.val) do
      when (go1.val) do
        let idxs = map (.val) indices1
        let (reqs, gids) = unzip (map (.val) reqs1)
        -- Apply per-bank lookup
        zipWithM_ (<==) reqs2 [reqs ! idx | idx <- idxs]
        -- Remember various things for the response path
        origActive2 <== origActive1.val
        zipWithM_ (<==) origBankTargets2 (map bankForReq reqs)
        zipWithM_ (<==) groupIds2 gids
        -- Trigger next stage
        go2 <== true
        active2 <== active1.val
        last2 <== last1.val
        mcastMask2 <== mcastMask1.val

  -- Stage 2: Issue loads
  -- ====================

  -- Decoded atomic operation
  atomicOps3 :: [Reg DecodedAtomicOp] <-
    replicateM SIMTSRAMBanks (makeReg dontCare)

  -- Per-bank logic for this stage
  let issueLoads sramBank req active decodedAMO = do
        if stallWire.val
          then do
            sramBank.preserveOutBE
          else do
            when (go2.val .&&. active) do
              -- Drop the bottom address bits used as bank selector
              -- Drop top bits outside memory map range for banked SRAMs
              let addr = truncate $
                    slice @31 @(SIMTLogSRAMBanks+2) (req.memReqAddr)
              -- Shorthand
              let amo = req.memReqAMOInfo.amoOp
              -- Check that memory operation is supported
              dynamicAssert (req.memReqOp .!=. memCacheFlushOp)
                "BankedSRAMs: cache flush not applicable"
              dynamicAssert (req.memReqOp .!=. memGlobalFenceOp)
                "BankedSRAMs: global fence not applicable"
              when (req.memReqOp .==. memAtomicOp) do
                dynamicAssert (amo .!=. amoLROp)
                  "BankedSRAMs: LR not supported"
                dynamicAssert (amo .!=. amoSCOp)
                  "BankedSRAMs: SC not supported"
              -- Decode atomic operation
              decodedAMO <== decodeAtomicOp amo
              -- Issue load to SRAM bank
              loadBE sramBank addr

  -- Instantiate per-bank logic for this stage
  always do
    sequence $ getZipList $
        issueLoads <$> ZipList sramBanks
                   <*> ZipList (map (.val) reqs2)
                   <*> ZipList (toBitList active2.val)
                   <*> ZipList atomicOps3

  always do 
    -- Trigger next stage
    when (inv stallWire.val) do
      when (go2.val) do
        go3 <== true
        active3 <== active2.val
        zipWithM_ (<==) reqs3 (map (.val) reqs2)
        origActive3 <== origActive2.val
        last3 <== last2.val
        mcastMask3 <== mcastMask2.val
        zipWithM_ (<==) origBankTargets3 (map (.val) origBankTargets2)
        zipWithM_ (<==) groupIds3 (map (.val) groupIds2)

  -- Stage 3: Issue stores
  -- =====================

  -- Wires to signal validity of each response
  respValidWires4 :: [Wire (Bit 1)] <-
    replicateM SIMTSRAMBanks (makeWire false)

  -- Per-bank logic for this stage
  let issueStores sramBank req active amo hasResp resp = do
        when (inv stallWire.val .&&. go3.val .&&. active) do
          -- Shorthands
          let a = req.memReqData
          let b = truncate sramBank.outBE
          -- Prepare min/max operation
          let msb a = upper a :: Bit 1
          let sa = amo.amo_isUnsigned ? (0, msb a)
          let sb = amo.amo_isUnsigned ? (0, msb b)
          let less = Signed (sa # a) .<. Signed (sb # b)
          let pickA = amo.amo_isMin .==. less
          -- Compute store data
          let storeData =
                select    
                  [ amo.amo_isSwap   --> a
                  , amo.amo_isAdd    --> a + b
                  , amo.amo_isXor    --> a .^. b
                  , amo.amo_isAnd    --> a .&. b
                  , amo.amo_isOr     --> a .|. b
                  , amo.amo_isMinMax --> if pickA then a else b
                  ]
          -- Issue response
          let needsResp = req.memReqOp .==. memLoadOp
                .||. (req.memReqOp .==. memAtomicOp .&&.
                        req.memReqAMOInfo.amoNeedsResp)
          when needsResp do
            resp <==
              MemResp {
                memRespId = req.memReqId
              , memRespData = truncate sramBank.outBE
              , memRespDataTagBit = at @32 (sramBank.outBE)
              , memRespIsFinal = req.memReqIsFinal
              }
            hasResp <== true
          -- Determine byte enable
          let byteEn = genByteEnable (req.memReqAccessWidth) (req.memReqAddr)
          -- Drop the bottom address bits used as bank selector
          let addr = truncate $
                       slice @31 @(SIMTLogSRAMBanks+2) (req.memReqAddr)
          -- Write to bank
          when (req.memReqOp .==. memStoreOp .||.
                  req.memReqOp .==. memAtomicOp) do
            let d = if req.memReqOp .==. memStoreOp
                      then req.memReqData
                      else storeData
            storeBE sramBank addr (EnableTaggedMem # byteEn)
              (dontCare # req.memReqDataTagBit # d)

  -- Instantiate per-bank logic for this stage
  always do
    sequence $ getZipList $
        issueStores <$> ZipList sramBanks
                    <*> ZipList (map (.val) reqs3)
                    <*> ZipList (toBitList active3.val)
                    <*> ZipList (map (.val) atomicOps3)
                    <*> ZipList respValidWires4
                    <*> ZipList resps4

  always do 
    -- Trigger next stage
    when (inv stallWire.val) do
      when (go3.val) do
        let active = active3.val .&. fromBitList (map (.val) respValidWires4)
        go4 <== (active .!=. 0)
        active4 <== origActive3.val
        last4 <== last3.val
        mcastMask4 <== mcastMask3.val
        zipWithM_ (<==) origBankTargets4 (map (.val) origBankTargets3)
        zipWithM_ (<==) groupIds4 (map (.val) groupIds3)

  -- Stage 4: Crossbar switch (for responses)
  -- ========================================

  always do
    when (inv stallWire.val .&&. go4.val) do
      -- Apply per-bank lookup
      let resps = map (.val) resps4
      zipWithM_ (<==) resps5 [resps ! idx | idx <- map (.val) origBankTargets4]
      -- Trigger next stage
      go5 <== true
      active5 <== active4.val
      last5 <== last4.val
      mcastMask5 <== mcastMask4.val
      zipWithM_ (<==) groupIds5 (map (.val) groupIds4)

  -- Stage 5: Issue responses
  -- ========================

  -- This stage is in one of two states: accumulate or respond
  let s_Accum :: Bit 1 = 0
  let s_Respond :: Bit 1 = 1
  state5 :: Reg (Bit 1) <- makeReg s_Accum

  -- Accummulated responses
  respsAccum :: [Reg (MemResp t_id)] <-
    replicateM SIMTLanes (makeReg dontCare)
  respsAccumValid :: Reg (Bit SIMTLanes) <- makeReg 0
  mcastMaskAccum :: Reg (Option (Bit SIMTLanes)) <- makeReg none

  always do
    -- Expand bank responses to lane respnses
    let numGroups = 2 ^ (SIMTLogLanes - SIMTLogSRAMBanks)
    let resps = concat $ replicate numGroups $ map (.val) resps5
    let gids_active = zip (map (.val) groupIds5) (toBitList active5.val)
    let active = concat
          [ [ act .&&. gid .==. fromInteger i
            | (gid, act) <- group ]
          | (i, group) <- zip [0..] (replicate numGroups gids_active) ]

    if state5.val .==. s_Respond
      then do
        -- Response state
        stallWire <== go5.val
        when respQueue.notFull do
          -- Issue response
          respQueue.enq $
            if mcastMaskAccum.val.valid
                 then fromList [ Option mcast (head respsAccum).val
                               | mcast <- toBitList mcastMaskAccum.val.val ]
                 else fromList [ Option valid resp.val
                               | (valid, resp) <-
                                   zip (toBitList respsAccumValid.val)
                                       respsAccum ]
          -- Clear accumulator
          respsAccumValid <== 0
          -- Move back to accumulation state
          state5 <== s_Accum
      else do
        -- Accumulate responses for original request set
        when go5.val do
          respsAccumValid <== respsAccumValid.val .|. fromBitList active
          sequence_
            [ when newValid do
                dynamicAssert (inv oldValid)
                  "BankedSRAMs: overwriting response (should be impossible)"
                accum <== resp
            | (newValid, oldValid, resp, accum) <-
                 zip4 active (toBitList respsAccumValid.val) resps respsAccum
            ]
          mcastMaskAccum <== mcastMask5.val
          -- When accumulation complete, move to next state
          when last5.val do
            state5 <== s_Respond

  return (toStream respQueue)

-- Note [Capability atomicity in banked SRAMs]
-- ===========================================

-- There may be serialisation of requests before they reach the SRAM
-- banks, breaking multi-flit atomic transactions (e.g. the first
-- flits from two different requests for the same location are
-- performed before the second flits of each).  See note [Memory
-- transactions].  This calls into question the atomicity of
-- capability writes.  However, provided that the client supplies
-- vectors of requests in such a way that requests from different
-- instructions (or different flits from the same instruction) are not
-- mixed (the intended behaviour of warp preservation), then the SRAM
-- banks are left in a consistent state.  In particular, the order of
-- the writes of the first halves will match the order of the writes
-- of the second halves.
