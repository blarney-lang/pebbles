module Pebbles.Instructions.MulUnit where

-- Blarney imports
import Blarney
import Blarney.SourceSink

-- Pebbles imports
import Pebbles.Pipeline.Interface

-- Request to multiplier unit
data MulReq =
  MulReq {
    -- Unique identifier from pipeline
    mulReqId :: InstrId
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

-- Multiplier unit (half throughput to save area)
-- To get a DSP multiplier, inputs and outputs must be registered
-- We assume the two input operands are already registered
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
              resumeReqId = reqReg.val.mulReqId
            , resumeReqData = reqReg.val.mulReqLower ?
                (resultReg.val.lower, resultReg.val.upper)
            }
        , canPeek = fullReg.val
        , consume = fullReg <== false
        }
    }
