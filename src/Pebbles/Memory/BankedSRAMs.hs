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

-- | Create an array of indepdendent SRAM banks that are accessible as a
-- multi-port shared memory.  We use a shuffle-exchange network to route
-- requests to (and responses from) the appropriate bank.
makeBankedSRAMs :: Bits t_id =>
     -- | Stream of memory requests per lane
     [Stream (MemReq t_id)]
     -- | Stream of memory responses per lane
  -> Module [Stream (MemResp t_id)]
makeBankedSRAMs reqStreams = do
  staticAssert (length reqStreams == SIMTLogLanes)
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
  sramBank :: RAM (Bit SIMTLogWordsPerSRAMBank) (Bit 32) <- makeRAM

  -- 2-element response queue
  respQueue :: Queue (MemResp t_id) <- makeQueue

  -- In-flight request counter
  -- The maximum count must equal the size of the response queue
  inflightCount :: Counter 2 <- makeCounter 2

  -- For triggering response enqueue on cycle after load is issued
  doEnq :: Reg (Bit 1) <- makeDReg false

  -- For the response id
  respIdReg :: Reg t_id <- makeReg dontCare

  -- Process requests
  always do
    when (reqs.canPeek .&. inflightCount.isFull.inv) do
      reqs.consume
      let req = reqs.peek
      -- Drop the bottom address bits used as bank selector
      let addr = truncate (slice @31 @(SIMTLogLanes+2) (req.memReqAddr))
      -- Pass request to block RAM 
      if req.memReqOp .==. memLoadOp
        then do
          load sramBank addr
          incrBy inflightCount 1
          respIdReg <== req.memReqId
          doEnq <== true
        else do
          store sramBank addr (req.memReqData)

  -- Produce responses
  always do
    when (doEnq.val) do
      dynamicAssert (respQueue.notFull)
        "makeSRAMBank: response queue overflow"
      enq respQueue
        MemResp {
          memRespId = respIdReg.val
        , memRespData = sramBank.out
        }
          
  return 
    Source {
      peek = respQueue.first
    , canPeek = respQueue.canDeq
    , consume = do
        respQueue.deq
        decrBy inflightCount 1
    }
