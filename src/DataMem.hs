module DataMem where

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.SourceSink

-- Local imports
import Pipeline

-- Processor/memory interface
-- ==========================

-- Memory request from the processor
data MemReq =
  MemReq {
    memReqId          :: InstrId
  , memReqIsStore     :: Bit 1
  , memReqAddr        :: Bit 32
  , memReqByteEn      :: Bit 4
  , memReqData        :: Bit 32
  , memReqAccessWidth :: Bit 2
  , memReqIsUnsigned  :: Bit 1
  } deriving (Generic, Bits)

-- A memory response to the processor
-- Includes a copy of the original request
type MemResp = (Bit 32, MemReq)

-- Memory unit interface
data MemUnit =
  MemUnit {
    memReqs :: Sink MemReq
  , memResps :: Source MemResp
  }

-- Data tightly-coupled memory (DTCM)
-- ==================================

-- One possible implementation of MemUnit, using a block RAM

-- DTCM size
type DTCMAddrWidth = 14

-- Tightly-coupled data memory with single-cycle read latency
makeDTCM :: Bool -> Module MemUnit
makeDTCM sim = do
  -- Data memory (block RAM with byte enables)
  let ext = if sim then ".hex" else ".mif"
  dataMem :: RAMBE DTCMAddrWidth 4 <- makeRAMInitBE ("data" ++ ext)

  -- Is response ready for consuming?
  ready :: Reg (Bit 1) <- makeReg false

  -- Current request
  reqWire :: Wire MemReq <- makeWire dontCare

  -- Ooriginal request
  reqReg :: Reg MemReq <- makeReg dontCare

  -- Wire pulsed when response is consumed
  doConsume :: Wire (Bit 1) <- makeWire false

  -- Can we consume another request?
  let full = ready.val .&. doConsume.val.inv

  always do
    -- Hold output of block RAM when full
    when full do
      dataMem.preserveOutBE
    -- Handle requests
    let req = reqWire.val
    when (reqWire.active) do
      reqReg <== req
      let addr = lower (upper (req.memReqAddr) :: Bit 30)
      if req.memReqIsStore
        then storeBE dataMem addr (req.memReqByteEn) (req.memReqData)
        else loadBE dataMem addr
    -- Update ready register
    ready <== full .|. (reqWire.active .&. req.memReqIsStore.inv)

  return
    MemUnit {
      memReqs =
        Sink {
          canPut = full.inv
        , put = (reqWire <==)
        }
    , memResps =
        Source {
          peek = (dataMem.outBE, reqReg.val)
        , canPeek = ready.val
        , consume = doConsume <== true
        }
    }
