module Pebbles.Memory.Interface where
  
-- Blarney imports
import Blarney
import Blarney.SourceSink

-- Local imports
import Pebbles.Pipeline.Interface

-- Processor/memory interface
-- ==========================

-- | Memory operation
type MemReqOp = Bit 2

-- | Memory request opcodes
memLoadOp       :: MemReqOp = 1
memStoreOp      :: MemReqOp = 2
memCacheFlushOp :: MemReqOp = 3

-- | Memory access width (bytes = 2 ^ AccessWidth)
type AccessWidth = Bit 2

-- | Byte, half-word, or word access?
isByteAccess, isHalfAccess, isWordAccess :: AccessWidth -> Bit 1
isByteAccess = (.==. 0b00)
isHalfAccess = (.==. 0b01)
isWordAccess = (.==. 0b10)

-- | Memory requests from the processor
data MemReq id =
  MemReq {
    -- | Identifier, to support out-of-order responses
    memReqId :: id
    -- | Access width of operation
  , memReqAccessWidth :: AccessWidth
    -- | Memory operation
  , memReqOp :: MemReqOp
    -- | Memory address
  , memReqAddr :: Bit 32
    -- | Data to store
  , memReqData :: Bit 32
    -- | Is it an unsigned load?
  , memReqIsUnsigned :: Bit 1
  } deriving (Generic, Interface, Bits)

-- | Memory responses to the processor
data MemResp id =
  MemResp {
    -- | Idenitifier, to match up request and response
    memRespId :: id
    -- | Response data
  , memRespData :: Bit 32
  } deriving (Generic, Interface, Bits)

-- | Memory unit interface
data MemUnit id =
  MemUnit {
    -- | Request sink
    memReqs :: Sink (MemReq id)
    -- | Response source
  , memResps :: Source (MemResp id)
  } deriving (Generic, Interface)

-- Helper functions
-- ================

-- | Convert memory response to pipeline resume request
memRespToResumeReq :: MemResp InstrInfo -> ResumeReq
memRespToResumeReq resp =
  ResumeReq {
    resumeReqInfo = resp.memRespId
  , resumeReqData = resp.memRespData
  }
