module Pebbles.SoC.DRAM.DualPort
  ( makeDRAMDualPort
  ) where

-- SoC parameters
#include <SoC.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.Interconnect

-- Pebbles imports
import Pebbles.SoC.DRAM.Wrapper
import Pebbles.SoC.DRAM.Interface

-- Types
-- =====

type Dual a = (a, a)

-- Dual-port DRAM
-- ==============

-- | Create a dual-port DRAM wrapper around a single DRAM instance.
-- Requests are merged fairly and responses are returned in order.
makeDRAMDualPort :: forall t_id. Bits t_id =>
     -- | DRAM request streams coming in
     Dual (Stream (DRAMReq t_id))
     -- | Avalon DRAM input signals
  -> AvalonDRAMIns
     -- | DRAM response streams, and Avalon DRAM output signals
  -> Module (Dual (Stream (DRAMResp t_id)), AvalonDRAMOuts)
makeDRAMDualPort (reqs0, reqs1) avlIns = do
  -- Tag a request with a client id
  let tag t req = req { dramReqId = (t, req.dramReqId) }

  -- Tag the request in each stream with a client id
  let dramReqs0 = fmap (tag 0) reqs0
  let dramReqs1 = fmap (tag 1) reqs1

  -- Merge buffer
  buffer <- makeQueue

  -- Input streams
  let inA = dramReqs0
  let inB = dramReqs1

  -- Locks on input streams (to avoid interleaving write bursts)
  lockA :: Reg (Bit 1) <- makeReg false
  lockB :: Reg (Bit 1) <- makeReg false

  -- Burst counts for each stream
  burstA :: Reg DRAMBurst <- makeReg 1
  burstB :: Reg DRAMBurst <- makeReg 1

  -- Was previous output taken from stream A?
  prevChoiceWasA :: Reg (Bit 1) <- makeReg false

  always do
    when (buffer.notFull) do
      -- Take next input from stream B?
      let chooseB = lockB.val .|.
            (lockA.val ? (false,
              inB.canPeek .&. (inA.canPeek.inv .|. prevChoiceWasA.val)))
      -- Consume input
      if chooseB
        then when (inB.canPeek) do
          inB.consume
          when (inB.peek.dramReqIsStore) do
            if burstB.val .==. inB.peek.dramReqBurst
              then do
                lockB <== false
                burstB <== 1
              else do
                lockB <== true
                burstB <== burstB.val + 1
        else when (inA.canPeek) do
          inA.consume
          when (inA.peek.dramReqIsStore) do
            if burstA.val .==. inA.peek.dramReqBurst
              then do
                lockA <== false
                burstA <== 1
              else do
                lockA <== true
                burstA <== burstA.val + 1
      -- Produce output
      when (inA.canPeek .|. inB.canPeek) do
        enq buffer (chooseB ? (inB.peek, inA.peek))
        prevChoiceWasA <== inv chooseB

  -- Single DRAM instance
  (dramResps, avlOuts) <- makeDRAM (buffer.toStream) avlIns

  -- Get the tag from the response
  let getTag resp = resp.dramRespId.fst

  -- Untag the response
  let untag resp = resp { dramRespId = resp.dramRespId.snd }

  -- Split into two response streams
  let resps0 =
        Source {
          peek = dramResps.peek.untag
        , canPeek = dramResps.canPeek .&. dramResps.peek.getTag.inv
        , consume = dramResps.consume
        }
  let resps1 =
        Source {
          peek = dramResps.peek.untag
        , canPeek = dramResps.canPeek .&. dramResps.peek.getTag
        , consume = dramResps.consume
        }

  return ((resps0, resps1), avlOuts)
