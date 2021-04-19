module Pebbles.Memory.BankedSRAMs where

-- SoC parameters
#include <SoC.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.Interconnect

-- Pebbles imports
import Pebbles.Util.Counter
import Pebbles.Memory.Interface
import Pebbles.Memory.Alignment

-- | SRAM bank request tag
data BankTag t_id =
  BankTag {
    bankReqId :: t_id
    -- ^ Request id
  , bankLaneId :: Bit SIMTLogLanes
    -- ^ Id of issuing lane
  , bankBroadcastResp :: Bit 1
    -- ^ Response should be sent to all lanes
  }
  deriving (Generic, Bits)

-- | Create an array of indepdendent SRAM banks that are accessible as a
-- multi-port shared memory.  We use a shuffle-exchange network to route
-- requests to (and responses from) the appropriate bank.
makeBankedSRAMs :: forall t_id. Bits t_id =>
     [Stream (MemReq t_id)]
     -- ^ Stream of memory requests per lane
  -> Module [Stream (MemResp t_id)]
     -- ^ Stream of memory responses per lane
makeBankedSRAMs reqStreams = do
  staticAssert (length reqStreams == SIMTLanes)
    "makeBankedSRAMs: number of streams /= number of lanes"

 -- Coalesce loads to the same address
  reqStreams0 <- makeSRAMCoalescer reqStreams

  -- On a fence, ensure one request per bank
  let remap (laneId :: Bit SIMTLogLanes) req =
        req {
          memReqAddr = if req.memReqOp .==. memLocalFenceOp
                         then 0 # laneId # (0b00 :: Bit 2)
                         else req.memReqAddr
        }
  let reqStreams1 = [ fmap (remap (fromInteger id)) s
                    | (s, id) <- zip reqStreams0 [0..] ]

  -- Shuffle-exchange network on requests
  let routeReq req = slice @(SIMTLogLanes+1) @2 (req.memReqAddr)
  let switchReq = makeFairExchange (makeShiftQueue 1)
  reqStreams2 <- makeShuffleExchange switchReq routeReq reqStreams1

  -- Instantiate SRAM banks
  respStreams0 <- mapM makeSRAMBank reqStreams2

  -- Shuffle-exchange network on responses
  -- Response network supports broadcast
  let routeResp resp = resp.memRespId.bankLaneId
  let bcastResp resp = resp.memRespId.bankBroadcastResp
  let switchResp = makeFairExchangeWithBroadcast (makeShiftQueue 1) bcastResp
  respStreams1 <- makeShuffleExchange switchResp routeResp respStreams0

  -- Drop tag in response
  let untag resp = resp { memRespId = resp.memRespId.bankReqId }
  let respStreams2 = [fmap untag s | s <- respStreams1]

  return respStreams2

-- | Create SRAM bank
makeSRAMBank :: Bits t_id =>
     Stream (MemReq t_id)
     -- ^ Stream of memory requests
  -> Module (Stream (MemResp t_id))
     -- ^ Stream of memory responses
makeSRAMBank reqs = do
  -- SRAM bank implemented by a block RAM
  sramBank :: RAMBE SIMTLogWordsPerSRAMBank 4 <- makeDualRAMBE

  -- Response queue
  respQueue :: Queue (MemResp t_id) <- makeShiftQueue 1

  -- SRAM bank state machine
  -- State 0: Consume request
  -- State 1: Issues response & update SRAM bank
  state :: Reg (Bit 1) <- makeReg 0

  -- Request register
  reqReg :: Reg (MemReq t_id) <- makeReg dontCare

  -- Decoded atomic operation
  isSwap     <- makeReg false
  isXor      <- makeReg false
  isAnd      <- makeReg false
  isAdd      <- makeReg false
  isOr       <- makeReg false
  isMin      <- makeReg false
  isMinMax   <- makeReg false
  isUnsigned <- makeReg false

  -- State 0: consume request
  always do
    when (reqs.canPeek .&&. state.val .==. 0) do
      reqs.consume
      let req = reqs.peek
      reqReg <== req
      -- Drop the bottom address bits used as bank selector
      let addr = truncate (slice @31 @(SIMTLogLanes+2) (req.memReqAddr))
      -- Shorthand
      let amo = req.memReqAMOInfo.amoOp
      -- Check that memory request is supported
      dynamicAssert (req.memReqOp .!=. memCacheFlushOp)
        "BankedSRAMs: cache flush not applicable!"
      dynamicAssert (req.memReqOp .!=. memGlobalFenceOp)
        "BankedSRAMs: global fence not applicable!"
      when (req.memReqOp .==. memAtomicOp) do
        dynamicAssert (amo .!=. amoLROp) "BankedSRAMs: LR not supported"
        dynamicAssert (amo .!=. amoSCOp) "BankedSRAMs: SC not supported"
      -- Decode atomic operation
      isSwap     <==  amo .==. amoSwapOp
      isXor      <==  amo .==. amoXorOp
      isAdd      <==  amo .==. amoAddOp
      isAnd      <==  amo .==. amoAndOp
      isOr       <==  amo .==. amoOrOp
      isMin      <==  orList [amo .==. op | op <- [amoMinOp, amoMinUOp]]
      isMinMax   <==  orList [amo .==. op | op <-
                        [amoMinOp, amoMaxOp, amoMinUOp, amoMaxUOp]]
      isUnsigned <==  orList [amo .==. op | op <- [amoMinUOp, amoMaxUOp]]
      -- Load data from block RAM and move to next state
      loadBE sramBank addr
      state <== 1
 
  -- State 1: issue response and update SRAM bank
  always do
    when (state.val .==. 1) do
      if respQueue.notFull
        then do
          -- Shorthands
          let req = reqReg.val
          let a = reqReg.val.memReqData
          let b = sramBank.outBE
          -- Prepare min/max operation
          let msb a = upper a :: Bit 1
          let sa = isUnsigned.val ? (0, msb a)
          let sb = isUnsigned.val ? (0, msb b)
          let less = (sa # a) `sLT` (sb # b)
          let pickA = isMin.val .==. less
          -- Compute store data
          let storeData =
                select    
                  [ isSwap.val   --> a
                  , isAdd.val    --> a + b
                  , isXor.val    --> a .^. b
                  , isAnd.val    --> a .&. b
                  , isOr.val     --> a .|. b
                  , isMinMax.val --> if pickA then a else b
                  ]
          -- Issue response
          let needsResp = req.memReqOp .==. memLoadOp
                .||. req.memReqOp .==. memLocalFenceOp
                .||. (req.memReqOp .==. memAtomicOp .&&.
                        req.memReqAMOInfo.amoNeedsResp)
          when needsResp do
            enq respQueue
              MemResp {
                memRespId = req.memReqId
              , memRespData = sramBank.outBE
              }
          -- Drop the bottom address bits used as bank selector
          let addr = truncate (slice @31 @(SIMTLogLanes+2) (req.memReqAddr))
          -- Determine byte enable
          let byteEn = genByteEnable (req.memReqAccessWidth) (req.memReqAddr)
          -- Write to bank
          when (req.memReqOp .==. memStoreOp .||.
                  req.memReqOp .==. memAtomicOp) do
            storeBE sramBank addr byteEn
              (if req.memReqOp .==. memStoreOp then req.memReqData
                                               else storeData)
          -- Move back to initial state
          state <== 0
        else do
          sramBank.preserveOutBE

  return (toStream respQueue)

-- | Simple coalescing stage to optimise the case where all lanes are
-- accessing the same SRAM address
makeSRAMCoalescer :: Bits t_id =>
     [Stream (MemReq t_id)]
     -- ^ Requests in
  -> Module [Stream (MemReq (BankTag t_id))]
     -- ^ Modified requests out
makeSRAMCoalescer ins = do
  -- Helper function to extract SRAM address bits
  -- (reducing the number of bits to compare)
  let getSRAMAddr req =
        req.memReqAddr.truncate :: Bit (SIMTLogWordsPerSRAMBank+2)

  -- Address of lane 0's request, for comparison
  let addr0 = ins.head.peek.getSRAMAddr

  -- Can the current batch of requests be coalesced?
  -- (i.e. are they all loads to the same address?)
  coalesce :: Reg (Bit 1) <- makeReg false

  -- Output queues
  outQueues :: [Queue (MemReq t_id)] <-
    replicateM SIMTLanes (makeShiftQueue 1)

  -- Observe when outputs are consumed
  consumeWires :: [Wire (Bit 1)] <-
    replicateM SIMTLanes (makeWire false)

  -- State machine
  -- 0: consume inputs
  -- 1: wait for outputs to be consumed
  state :: Reg (Bit 1) <- makeReg 0

  -- Consume inputs
  always do
    -- State 0: consume inputs
    when (state.val .==. 0) do
      -- Can the current batch of requests be coalesced?
      coalesce <== andList
        [ s.canPeek .&&.
            s.peek.memReqOp .==. memLoadOp .&&.
              s.peek.getSRAMAddr .==. addr0
        | s <- ins ]
      -- Consume requests
      sequence
        [ when (s.canPeek) do
            s.consume
            enq q (s.peek)
        | (s, q) <- zip ins outQueues ]
      -- Move to next state when any requests are available
      when (orList (map canPeek ins)) do
        state <== 1

    -- State 1: wait for outputs to be consumed
    when (state.val .==. 1) do
      if coalesce.val
        then do
          -- Drop coalesced requests
          sequence [when (q.canDeq) do q.deq | q <- drop 1 outQueues]
          -- Return to state 0 when first & only request is consumed
          when (consumeWires.head.val) do
            state <== 0
        else do
          -- Return to state 0 when all requests have been consumed
          let waiting = orList
                [ q.canDeq .&&. w.val.inv
                | (q, w) <- zip outQueues consumeWires ]
          when (inv waiting) do
            state <== 0

  -- Output streams
  let outStreams =
        [ Source {
            peek =
              (q.first) {
                memReqId =
                  BankTag {
                      bankReqId = q.first.memReqId
                    , bankLaneId = fromInteger i
                    , bankBroadcastResp =
                        if i > 0 then false else coalesce.val
                  }
              }
          , canPeek =
              if i > 0
                then coalesce.val.inv .&&. q.canDeq
                else q.canDeq
          , consume = do
              w <== true
              deq q
          }
        | (q, w, i) <- zip3 outQueues consumeWires [0..] ]

  return outStreams
