module Pebbles.Memory.DRAM.Wrapper
  ( makeDRAM
  , makeDRAMUnstoppable
  ) where

-- SoC parameters
#include <Config.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream

-- Pebbles imports
import Pebbles.Util.Counter
import Pebbles.Memory.DRAM.Interface

-- Types
-- =====

-- Used internally
data DRAMInFlightReq id =
  DRAMInFlightReq {
    dramInFlightId :: id
  , dramInFlightBurst :: DRAMBurst
  } deriving (Generic, Bits)

-- DRAM wrapper core
-- =================

-- Used internally
makeDRAMCore :: Bits t_id =>
     -- Type of queue used to buffer responses
     (forall a. Bits a => Module (Queue a))
     -- DRAM requests to be consumed 
  -> Stream (DRAMReq t_id)
     -- Avalon DRAM input signals
  -> AvalonDRAMIns
     -- DRAM responses produced, and Avalon DRAM output signals
  -> Module (Stream (DRAMResp t_id), AvalonDRAMOuts, DRAMStatSigs)
makeDRAMCore makeRespQueue reqs avlIns =
  do -- Queue of in-flight requests
     inFlight :: Queue (DRAMInFlightReq t_id) <-
       makeSizedQueue DRAMLogMaxInFlight

     -- Response queue
     resps :: Queue DRAMBeat <- makeRespQueue

     -- In-flight request counter
     inFlightCount :: Counter (DRAMLogMaxInFlight+1) <-
       makeCounter (fromInteger (2^DRAMLogMaxInFlight))

     -- Registers
     address    :: Reg DRAMAddr   <- makeReg dontCare
     writeData  :: Reg DRAMBeat   <- makeReg dontCare
     byteEn     :: Reg DRAMByteEn <- makeReg dontCare
     doRead     :: Reg (Bit 1)    <- makeReg 0
     doWrite    :: Reg (Bit 1)    <- makeReg 0
     burstReg   :: Reg DRAMBurst  <- makeReg 0
     burstCount :: Reg DRAMBurst  <- makeReg 1

     -- Wires
     putLoad     :: Wire (Bit 1) <- makeWire 0
     putStore    :: Wire (Bit 1) <- makeWire 0

     -- Wait request
     let waitRequest = avlIns.avl_dram_waitrequest

     -- Maximum burst size
     let maxBurst = fromInteger (2 ^ (DRAMBurstWidth-1))

     -- Put requests to external DRAM
     always do
       when (putLoad.val) do
         doRead <== 1
         doWrite <== 0
       when (putStore.val) do
         doRead <== 0
         doWrite <== 1
       when (inv putLoad.val .&. inv putStore.val .&. inv waitRequest) do
         doRead <== 0
         doWrite <== 0

     -- Next request
     let req :: DRAMReq t_id = reqs.peek

     -- Process DRAM requests
     always do
       when (reqs.canPeek .&. inv waitRequest) do
         -- Check burst size
         dynamicAssert (req.dramReqBurst .<=. maxBurst)
           "DRAM: max burst size exceeded"
         -- Consume request
         when (inFlightCount.getAvailable .>=. zeroExtend req.dramReqBurst) do
           reqs.consume
           byteEn <== req.dramReqByteEn
           address <== req.dramReqAddr
           burstReg <== req.dramReqBurst
           writeData <== req.dramReqData
           if req.dramReqIsStore then putStore <== 1 else putLoad <== 1
           when (inv req.dramReqIsStore) do
             dynamicAssert (inFlight.notFull) "DRAM: enqueueing to full queue"
             enq inFlight 
               DRAMInFlightReq {
                 dramInFlightId = req.dramReqId
               , dramInFlightBurst = req.dramReqBurst
               }
             inFlightCount `incrBy` zeroExtend (req.dramReqBurst)

     -- Process DRAM responses
     always do
       when (avlIns.avl_dram_readdatavalid) do
         dynamicAssert (resps.notFull) "DRAM: enqueueing to full queue"
         enq resps (avlIns.avl_dram_readdata)

     -- Prepare the response stream
     let respsOut =
           Source {
             canPeek = inFlight.canDeq .&. resps.canDeq
           , peek =
               DRAMResp {
                 dramRespId = inFlight.first.dramInFlightId
               , dramRespBurstId = burstCount.val
               , dramRespData = resps.first
               , dramRespDataTagBits = 0
               }
           , consume = do
               dynamicAssert (inFlight.canDeq .&. resps.canDeq)
                 "DRAM: consuming from empty queue"
               resps.deq
               inFlightCount `decrBy` 1
               if burstCount.val .==. inFlight.first.dramInFlightBurst
                 then do
                   inFlight.deq
                   burstCount <== 1
                 else do
                   burstCount <== burstCount.val + 1
           }

     -- Prepare Avalon output signals
     let avlOuts =
           AvalonDRAMOuts {
             avl_dram_read       = doRead.val
           , avl_dram_write      = doWrite.val
           , avl_dram_writedata  = writeData.val
           , avl_dram_address    = address.val
           , avl_dram_byteen     = byteEn.val
           , avl_dram_burstcount = zeroExtend (burstReg.val)
           }

     -- Stats interface
     let stats =
           DRAMStatSigs {
             dramLoadSig  = putLoad.val ? (req.dramReqBurst, 0)
           , dramStoreSig = zeroExtend putStore.val
           }

     return (respsOut, avlOuts, stats)

-- DRAM wrapper supporting backpressure on responses
-- =================================================

-- | Create a convenient DRAM wrapper.  Responses are returned in order.
makeDRAM :: Bits t_id =>
     -- | DRAM requests to be consumed 
     Stream (DRAMReq t_id)
     -- | Avalon DRAM input signals
  -> AvalonDRAMIns
     -- | DRAM responses produced, and Avalon DRAM output signals
  -> Module (Stream (DRAMResp t_id), AvalonDRAMOuts, DRAMStatSigs)
makeDRAM reqs avlIns =
  -- Use a response queue big enough to guarantee no overflow
  makeDRAMCore (makeSizedQueue DRAMLogMaxInFlight) reqs avlIns

-- DRAM wrapper without backpressure on responses
-- ==============================================

-- | Create a convenient DRAM wrapper.  Responses are returned in order.
-- Responses must be consumed immediately.
makeDRAMUnstoppable :: Bits t_id =>
     -- | DRAM requests to be consumed 
     Stream (DRAMReq t_id)
     -- | Avalon DRAM input signals
  -> AvalonDRAMIns
     -- | DRAM responses produced, and Avalon DRAM output signals
  -> Module (Stream (DRAMResp t_id), AvalonDRAMOuts, DRAMStatSigs)
makeDRAMUnstoppable reqs avlIns =
  -- Use a small response queue which may overflow if
  -- responses are not consumed fast enough
  makeDRAMCore (makePipelineQueue 1) reqs avlIns
