module Pebbles.Instructions.Units.DivUnit where

-- Blarney imports
import Blarney
import Blarney.Stmt
import Blarney.SourceSink

-- Pebbles imports
import Pebbles.Pipeline.Interface

-- Request to divider unit
data DivReq =
  DivReq {
    -- Instruction info from pipeline
    divReqInfo :: InstrInfo
    -- Numerator and denominator
  , divReqNum :: Bit 32
  , divReqDenom :: Bit 32
    -- Signed or unsigned division?
  , divReqIsSigned :: Bit 1
    -- Do we want the quotient or remainder?
  , divReqGetRemainder :: Bit 1
  } deriving (Generic, Bits)

-- Divider unit interface
data DivUnit =
  DivUnit {
    divReqs :: Sink DivReq
  , divResps :: Source ResumeReq
  }

-- Divider unit (sequential state machine version)
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
            }
        , consume = do done <== false
        }
    }
