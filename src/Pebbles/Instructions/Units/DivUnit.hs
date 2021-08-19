module Pebbles.Instructions.Units.DivUnit where

-- Blarney imports
import Blarney
import Blarney.Stmt
import Blarney.Queue
import Blarney.Option
import Blarney.SourceSink
import Blarney.Core.BV
import Blarney.VendorIP.PipelinedDivider

-- Pebbles imports
import Pebbles.Util.List
import Pebbles.Util.Counter
import Pebbles.Pipeline.Interface

-- | Request to divider unit
data DivReq =
  DivReq {
    divReqInfo :: InstrInfo
    -- ^ Instruction info from pipeline
  , divReqNum :: Bit 32
  , divReqDenom :: Bit 32
    -- ^ Numerator and denominator
  , divReqIsSigned :: Bit 1
    -- ^ Signed or unsigned division?
  , divReqGetRemainder :: Bit 1
    -- ^ Do we want the quotient or remainder?
  } deriving (Generic, Bits)

-- | Divider unit interface
data DivUnit =
  DivUnit {
    divReqs :: Sink DivReq
  , divResps :: Source ResumeReq
  }

-- Sequential divider
-- ==================

-- | Divider unit (sequential state machine version)
makeSeqDivUnit :: Module DivUnit
makeSeqDivUnit = do
  -- Numerator, denominator, quotient, and remainder
  n :: Reg (Bit 32) <- makeReg dontCare
  d :: Reg (Bit 32) <- makeReg dontCare
  q :: Reg (Bit 32) <- makeReg dontCare
  r :: Reg (Bit 32) <- makeReg dontCare

  -- Counter
  count :: Reg (Bit 6) <- makeReg dontCare

  -- Is the result ready for consumption?
  done :: Reg (Bit 1) <- makeReg false

  -- Is the result currently being computed?
  busy :: Reg (Bit 1) <- makeReg false

  -- Trigger division state machine
  trigger :: Reg (Bit 1) <- makeDReg false

  -- Signed or unsigned division?
  isSigned :: Reg (Bit 1) <- makeReg dontCare

  -- Quotient or remainder?
  getRemainder :: Reg (Bit 1) <- makeReg dontCare

  -- Flip the sign of the result?
  negResult :: Reg (Bit 1) <- makeReg dontCare

  -- Output register
  output :: Reg (Bit 32) <- makeReg dontCare

  -- Remember the request info
  reqInfo :: Reg InstrInfo <- makeReg dontCare

  -- Helper function to shift left by one
  let shl x = x .<<. (1 :: Bit 1)

  -- State machine
  runStmtOn (trigger.val) do
    -- Prepare division
    action do
      let divByZero = d.val .==. 0
      let n_msb = at @31 (n.val)
      let d_msb = at @31 (d.val)
      -- Negate numerator and denominator if required
      when (isSigned.val .&. divByZero.inv) do
        when n_msb do
          n <== n.val.negate
        when d_msb do
          d <== d.val.negate
      -- Initialise state machine
      q <== 0
      r <== 0
      count <== 32
      -- Negate result?
      negResult <== isSigned.val .&. divByZero.inv .&.
        (getRemainder.val ? (n_msb, n_msb .^. d_msb))
    -- Binary long division algorithm (taken from Wikipedia)
    while (count.val .!=. 0) do
      action do
        let r' = r.val.shl .|. zeroExtend (at @31 (n.val))
        let sub = r' .>=. d.val
        r <== r' - (sub ? (d.val, 0))
        q <== q.val.shl .|. (sub ? (1, 0))
        n <== n.val.shl
        count <== count.val - 1
    -- Prepare result
    action do
      done <== true
      busy <== false
      -- Choose quotient or remainder, and optionally negate
      let result = getRemainder.val ? (r.val, q.val)
      output <== negResult.val ? (result.negate, result)

  return
    DivUnit {
      divReqs =
        Sink {
          canPut = busy.val.inv .&. done.val.inv
        , put = \req -> do
            reqInfo <== req.divReqInfo
            n <== req.divReqNum
            d <== req.divReqDenom
            isSigned <== req.divReqIsSigned
            getRemainder <== req.divReqGetRemainder
            busy <== true
            trigger <== true
        }
    , divResps =
        Source {
          canPeek = done.val
        , peek =
            ResumeReq {
              resumeReqInfo = reqInfo.val
            , resumeReqData = output.val
            , resumeReqCap = none
            }
        , consume = do done <== false
        }
    }

-- Full-throughput divider
-- =======================

-- | Full throughput divider unit (using Blarney IP)
makeFullDivUnit :: Int -> Module DivUnit
makeFullDivUnit latency
    -- Check that latency is small enough to allow result queue to fit in MLABs
  | latency >= 32 = error "makeFullDivUnit: latency too large"
  | otherwise = do

      -- Result and request info queues
      resultQueue :: Queue (Bit 32) <- makeSizedQueue 5
      infoQueue :: Queue InstrInfo <- makeSizedQueue 5

      -- Track size of the infoQueue
      inflightCount :: Counter 6 <- makeCounter 32

      -- Wires (numerator, denominator, want remainder)
      numWire :: Wire (Bit 33) <- makeWire dontCare
      denomWire :: Wire (Bit 33) <- makeWire dontCare
      remWire :: Wire (Bit 1) <- makeWire dontCare

      always do
        -- Output of pipelined divider megafunction
        let (quot, rem) =
              pipelinedDivider latency (numWire.val) (denomWire.val)
        -- Wait 'latency' cycles for result to be ready
        let (ready, wantRem, divByZero) =
              iterateN latency
                (delay (false, false, false))
                (remWire.active, remWire.val, denomWire.val .==. 0)
        -- Fill result queue
        when ready do
          let result =
                if wantRem
                  then rem
                  else divByZero ? (ones, quot)
          -- Result queue must have space
          dynamicAssert (resultQueue.notFull) "DivUnit: result queue overflow"
          -- Enqueue result
          enq resultQueue (truncate result)

      return
        DivUnit {
          divReqs =
            Sink {
              canPut = inflightCount.isFull.inv
            , put = \req -> do
                incrBy inflightCount 1
                enq infoQueue (req.divReqInfo)
                -- Extend inputs to 33 bits
                let numExt = req.divReqIsSigned ?
                               (req.divReqNum.upper, false)
                let denomExt = req.divReqIsSigned ?
                                 (req.divReqDenom.upper, false)
                numWire <== numExt # req.divReqNum
                denomWire <== denomExt # req.divReqDenom
                remWire <== req.divReqGetRemainder

            }
        , divResps =
            Source {
              canPeek = resultQueue.canDeq .&&. infoQueue.canDeq
            , peek =
                ResumeReq {
                  resumeReqInfo = infoQueue.first
                , resumeReqData = resultQueue.first
                , resumeReqCap = none
                }
            , consume = do
                resultQueue.deq
                infoQueue.deq
                decrBy inflightCount 1
            }
        }
