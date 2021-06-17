module Pebbles.Memory.Interface where
  
-- Blarney imports
import Blarney
import Blarney.SourceSink

-- Local imports
import Pebbles.Pipeline.Interface

-- Interface to memory subsystem
-- =============================

-- | Memory operation
type MemReqOp = Bit 3

-- | Memory request opcodes
memCacheFlushOp  :: MemReqOp = 0
memLoadOp        :: MemReqOp = 1
memStoreOp       :: MemReqOp = 2
memAtomicOp      :: MemReqOp = 3
memGlobalFenceOp :: MemReqOp = 4
memLocalFenceOp  :: MemReqOp = 5

-- | Information about an atomic memory operation
data AMOInfo =
  AMOInfo {
    amoOp :: AtomicOp
    -- ^ Atomic operation opcode
  , amoAcquire :: Bit 1
  , amoRelease :: Bit 1
  , amoNeedsResp :: Bit 1
    -- ^ Is the response of the atomic operation needed?
  }
  deriving (Generic, Interface, Bits)

-- | Atomic operation
type AtomicOp = Bit 5

-- | Atomic opcodes (use the RISC-V encoding)
-- Atomic operations are only valid for word-sized accesses
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

-- | Byte, half-word, or word word access?
isByteAccess, isHalfAccess, isWordAccess :: AccessWidth -> Bit 1
isByteAccess = (.==. 0b00)
isHalfAccess = (.==. 0b01)
isWordAccess = (.==. 0b10)

-- | Memory requests from the processor
data MemReq id =
  MemReq {
    memReqId :: id
    -- ^ Identifier, to support out-of-order responses
  , memReqAccessWidth :: AccessWidth
    -- ^ Access width of operation
  , memReqOp :: MemReqOp
    -- ^ Memory operation
  , memReqAMOInfo :: AMOInfo
    -- ^ Atomic operation info (if memory operation is atomic)
  , memReqAddr :: Bit 32
    -- ^ Memory address
  , memReqData :: Bit 32
    -- ^ Data to store
  , memReqDataTagBit :: Bit 1
    -- ^ Data tag bit
  , memReqIsUnsigned :: Bit 1
    -- ^ Is it an unsigned load?
  , memReqIsFinal :: Bit 1
    -- ^ Is this the final request of an atomic transaction?
    -- See note [Memory transactions]
  } deriving (Generic, Interface, Bits)

-- | Memory responses to the processor
data MemResp id =
  MemResp {
    memRespId :: id
    -- ^ Idenitifier, to match up request and response
  , memRespData :: Bit 32
    -- ^ Response data
  , memRespDataTagBit :: Bit 1
    -- ^ Data tag bit
  } deriving (Generic, Interface, Bits)

-- | Interface to the memory unit
data MemUnit id =
  MemUnit {
    memReqs :: Sink (MemReq id)
    -- ^ Request sink
  , memResps :: Source (MemResp id)
    -- ^ Response source
  } deriving (Generic, Interface)

-- | Information from a memory request needed to process response
data MemReqInfo =
  MemReqInfo {
    memReqInfoAddr :: Bit 2
    -- ^ Lower bits of address
  , memReqInfoAccessWidth :: AccessWidth
    -- ^ Access width
  , memReqInfoIsUnsigned :: Bit 1
    -- ^ Is it an unsigned load?
  }
  deriving (Generic, Interface, Bits)

-- Helper functions
-- ================

-- | Convert memory response to pipeline resume request
memRespToResumeReq :: MemResp InstrInfo -> ResumeReq
memRespToResumeReq resp =
  ResumeReq {
    resumeReqInfo = resp.memRespId
  , resumeReqData = resp.memRespData
  }

-- Note [Memory transactions]
-- ==========================

-- Memory transactions are sequences of requests that cannot be
-- interleaved with other memory requests.  The scope of a transaction
-- is simply defined using the 'memReqIsFinal' bit.  The main intent
-- here is to support accessing data items larger than 32 bits in an
-- atomic manner.  Of course, the mechanism can be abused to hog
-- busses for long durations, impacting overall system efficiency, so
-- use with care!
