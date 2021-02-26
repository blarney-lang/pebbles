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

-- | Create an array of indepdendent SRAM banks that are accessible as a
-- multi-port shared memory.  We use a shuffle-exchange network to route
-- requests to (and responses from) the appropriate bank.
makeBankedSRAMs :: Bits t_id =>
     -- | Stream of memory requests per lane
     [Stream (MemReq t_id)]
     -- | Stream of memory responses per lane
  -> Module [Stream (MemResp t_id)]
makeBankedSRAMs reqStreams = do
  staticAssert (length reqStreams == SIMTLanes)
    "makeBankedSRAMs: number of streams /= number of lanes"

  -- Tag requests with lane id
  let tag t req = req { memReqId = (t :: Bit SIMTLogLanes, req.memReqId) }
  let reqStreams1 = [ fmap (tag (fromInteger id)) s
                    | (s, id) <- zip reqStreams [0..] ]

  -- Shuffle-exchange network on requests
  let routeReq req = slice @(SIMTLogLanes+1) @2 (req.memReqAddr)
  reqStreams2 <- makeShuffleExchange (makeShiftQueue 1) routeReq reqStreams1

  -- Instantiate SRAM banks
  respStreams <- mapM makeSRAMBank reqStreams2

  -- Shuffle-exchange network on responses
  let routeResp resp = resp.memRespId.fst
  respStreams1 <- makeShuffleExchange (makeShiftQueue 1) routeResp respStreams

  -- Drop tag in response
  let untag resp = resp { memRespId = resp.memRespId.snd }
  let respStreams2 = [fmap untag s | s <- respStreams1]

  return respStreams2

-- | Create SRAM bank
makeSRAMBank :: Bits t_id =>
     -- | Stream of memory requests
     Stream (MemReq t_id)
     -- | Stream of memory responses
  -> Module (Stream (MemResp t_id))
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
    when (state.val .==. 0) do
      reqs.consume
      let req = reqs.peek
      reqReg <== req
      -- Drop the bottom address bits used as bank selector
      let addr = truncate (slice @31 @(SIMTLogLanes+2) (req.memReqAddr))
      -- Shorthand
      let amo = req.memReqAtomicInfo.amoOp
      -- Check that memory request is supported
      dynamicAssert (req.memReqOp .!=. memCacheFlushOp)
        "BankedSRAMs: cache flush not applicable!"
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
      when (respQueue.notFull) do
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
        when (req.memReqOp .!=. memStoreOp) do
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
        when (req.memReqOp .!=. memLoadOp) do
          storeBE sramBank addr byteEn
            (if req.memReqOp .==. memStoreOp then req.memReqData
                                             else storeData)
        -- Move back to initial state
        state <== 0

  return (toStream respQueue)
