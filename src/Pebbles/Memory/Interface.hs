module Pebbles.Memory.Interface where
  
-- Blarney imports
import Blarney
import Blarney.SourceSink

-- Local imports
import Pebbles.Pipeline.Interface

-- Processor/memory interface
-- ==========================

-- | Memory requests from the processor
data MemReq id =
  MemReq {
    -- | Identifier, to support out-of-order responses
    memReqId :: id
    -- | Is it a store requets
  , memReqIsStore :: Bit 1
    -- | Memory address
  , memReqAddr :: Bit 32
    -- | Byte enable for stores
  , memReqByteEn :: Bit 4
    -- | Data to store
  , memReqData :: Bit 32
  } deriving (Generic, Bits)

-- | Memory responses to the processor
data MemResp id =
  MemResp {
    -- | Idenitifier, to match up request and response
    memRespId :: id
    -- | Response data
  , memRespData :: Bit 32
  } deriving (Generic, Bits)

-- | Memory unit interface
data MemUnit id =
  MemUnit {
    -- | Request sink
    memReqs :: Sink (MemReq id)
    -- | Response source
  , memResps :: Source (MemResp id)
  }
