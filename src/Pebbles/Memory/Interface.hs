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
memCacheFlushOp :: MemReqOp = 0
memLoadOp       :: MemReqOp = 1
memStoreOp      :: MemReqOp = 2
memAtomicOp     :: MemReqOp = 3

-- | Information about an atomic memory operation
data AtomicInfo =
  AtomicInfo {
    amoOp :: AtomicOp
  , amoAcquire :: Bit 1
  , amoRelease :: Bit 1
  }
  deriving (Generic, Interface, Bits)

-- | Atomic operation
type AtomicOp = Bit 5

-- | Atomic opcodes (use the RISC-V encoding)
amoLROp   :: AtomicOp = 0b00010
amoSCOp   :: AtomicOp = 0b00011
amoSwapOp :: AtomicOp = 0b00001
amoAddOp  :: AtomicOp = 0b00000
amoXorOp  :: AtomicOp = 0b00100
amoAndOp  :: AtomicOp = 0b01100
amoOrOp   :: AtomicOp = 0b01000
amoMinOp  :: AtomicOp = 0b10000
amoMaxOp  :: AtomicOp = 0b10100
amoMinUOp :: AtomicOp = 0b11000
amoMaxUOp :: AtomicOp = 0b11100

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
    -- | Atomic operation (if memory operation is atomic)
  , memReqAtomicInfo :: AtomicInfo
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

-- | Information from a memory request needed to process response
data MemReqInfo =
  MemReqInfo {
    -- | Lower bits of address
    memReqInfoAddr :: Bit 2
    -- | Access width
  , memReqInfoAccessWidth :: AccessWidth
    -- | Is it an unsigned load?
  , memReqInfoIsUnsigned :: Bit 1
  }
  deriving (Generic, Bits)

-- Helper functions
-- ================

-- | Convert memory response to pipeline resume request
memRespToResumeReq :: MemResp InstrInfo -> ResumeReq
memRespToResumeReq resp =
  ResumeReq {
    resumeReqInfo = resp.memRespId
  , resumeReqData = resp.memRespData
  }
