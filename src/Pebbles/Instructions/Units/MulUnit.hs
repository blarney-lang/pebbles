module Pebbles.Instructions.Units.MulUnit where

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.Stream
import Blarney.SourceSink

-- Pebbles imports
import Pebbles.Pipeline.Interface

-- Interface
-- =========

-- Request to multiplier unit
data MulReq =
  MulReq {
    -- Instruction info from pipeline
    mulReqInfo :: InstrInfo
    -- Operands to multiply
  , mulReqA :: Bit 32
  , mulReqB :: Bit 32
    -- Do we want the lower (or upper) bits of the result?
  , mulReqLower :: Bit 1
    -- Are the operands signed or unsigned?
  , mulReqUnsignedA :: Bit 1
  , mulReqUnsignedB :: Bit 1
  } deriving (Generic, Bits)

-- Multiplier unit interface
data MulUnit =
  MulUnit {
    mulReqs :: Sink MulReq
  , mulResps :: Source ResumeReq
  }

-- Half throughput multiplier
-- ==========================

-- Half throughput
makeHalfMulUnit :: Module MulUnit
makeHalfMulUnit = do
  -- Registers for request and result
  reqReg <- makeReg dontCare
  resultReg <- makeReg dontCare

  -- Do we have space to store the result?
  fullReg <- makeReg false

  return
    MulUnit {
      mulReqs =
        Sink {
          canPut = fullReg.val.inv
        , put = \req -> do
            let msbA = at @31 (req.mulReqA)
            let msbB = at @31 (req.mulReqB)
            let extA = req.mulReqUnsignedA ? (0, msbA)
            let extB = req.mulReqUnsignedB ? (0, msbB)
            let mulA = extA # req.mulReqA
            let mulB = extB # req.mulReqB
            reqReg <== req
            resultReg <== slice @63 @0 (fullMul True mulA mulB)
            fullReg <== true
        }
    , mulResps =
        Source {
          peek = 
            ResumeReq {
              resumeReqInfo = reqReg.val.mulReqInfo
            , resumeReqData = reqReg.val.mulReqLower ?
                (resultReg.val.lower, resultReg.val.upper)
            , resumeReqCap = none
            }
        , canPeek = fullReg.val
        , consume = fullReg <== false
        }
    }

-- Full throughput multiplier
-- ==========================

-- Full throughput, pipelined, optimised for Fmax
makeFullMulUnit :: Module MulUnit
makeFullMulUnit = do
  -- Stall wire
  stall :: Wire (Bit 1) <- makeWire false

  -- Pipeline stage triggers
  go1 :: Reg (Bit 1) <- makeDReg false
  go2 :: Reg (Bit 1) <- makeDReg false
  go3 :: Reg (Bit 1) <- makeDReg false

  -- Request register for each pipeline stage
  req1 :: Reg MulReq <- makeReg dontCare
  req2 :: Reg MulReq <- makeReg dontCare
  req3 :: Reg MulReq <- makeReg dontCare

  -- Operands (used in stage 1)
  lowerA :: Reg (Bit 17) <- makeReg dontCare
  upperA :: Reg (Bit 17) <- makeReg dontCare
  lowerB :: Reg (Bit 17) <- makeReg dontCare
  upperB :: Reg (Bit 17) <- makeReg dontCare

  -- Partial products (used in stage 2)
  prod0 :: Reg (Bit 34) <- makeReg dontCare
  prod1 :: Reg (Bit 34) <- makeReg dontCare
  prod2 :: Reg (Bit 34) <- makeReg dontCare

  -- Result register (used in stage 3)
  sum :: Reg (Bit 64) <- makeReg dontCare

  -- Result queue
  resultQueue :: Queue ResumeReq <- makeQueue

  -- Replay stages on stall
  always do
    when (stall.val) do
      go1 <== go1.val
      go2 <== go2.val
      go3 <== go3.val

  -- Stage 1: compute partial products
  always do
    when (go1.val .&. stall.val.inv) do
      prod0 <== fullMul True (lowerA.val) (lowerB.val)
      prod1 <== fullMul True (lowerA.val) (upperB.val) +
                  fullMul True (upperA.val) (lowerB.val)
      prod2 <== fullMul True (upperA.val) (upperB.val)
      req2 <== req1.val
      go2 <== true

  -- Stage 2: sum partial products
  always do
    when (go2.val .&. stall.val.inv) do
      sum <== signExtend (prod0.val)
                + (signExtend (prod1.val) # (0 :: Bit 16))
                + truncate (prod2.val # (0 :: Bit 32))
      req3 <== req2.val
      go3 <== true

  -- Stage 3: select result
  always do
    when (go3.val) do
      if resultQueue.notFull
        then do
          enq resultQueue
            ResumeReq {
              resumeReqInfo = req3.val.mulReqInfo
            , resumeReqData = req3.val.mulReqLower ?
                (sum.val.lower, sum.val.upper)
            , resumeReqCap = none
            }
      else do
        stall <== true

  return
    MulUnit {
      mulReqs =
        Sink {
          canPut = stall.val.inv
        , put = \req -> do
            let msbA = at @31 (req.mulReqA)
            let msbB = at @31 (req.mulReqB)
            let extA = req.mulReqUnsignedA ? (0, msbA)
            let extB = req.mulReqUnsignedB ? (0, msbB)
            lowerA <== false # slice @15 @0 (req.mulReqA)
            lowerB <== false # slice @15 @0 (req.mulReqB)
            upperA <== extA # slice @31 @16 (req.mulReqA)
            upperB <== extB # slice @31 @16 (req.mulReqB)
            req1 <== req
            go1 <== true
        }
    , mulResps = resultQueue.toStream
    }
