module Pebbles.Memory.Interface where

-- Blarney imports
import Blarney
import Blarney.SourceSink

-- Local imports
import Pebbles.Pipeline.Interface

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
data MemResp =
  MemResp {
    -- Response data
    memRespData :: Bit 32
    -- A copy of the original request
  , memRespInfo :: MemReq
  } deriving (Generic, Bits)

-- Memory unit interface
data MemUnit =
  MemUnit {
    memReqs :: Sink MemReq
  , memResps :: Source MemResp
  }
