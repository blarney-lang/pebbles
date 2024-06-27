module Pebbles.Memory.Interface where
  
-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.Option
import Blarney.SourceSink

-- Local imports
import CHERI.CapLib
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
data MemReq =
  MemReq {
    memReqAccessWidth :: AccessWidth
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
  , memReqDataTagBitMask :: Bit 1
    -- ^ Mask to apply to tag bit on response
  , memReqIsUnsigned :: Bit 1
    -- ^ Is it an unsigned load?
  , memReqIsFinal :: Bit 1
    -- ^ Is this the final request of an atomic transaction?
    -- See note [Memory transactions]
  } deriving (Generic, Interface, Bits)

-- | Memory responses to the processor
data MemResp =
  MemResp {
    memRespData :: Bit 32
    -- ^ Response data
  , memRespDataTagBit :: Bit 1
    -- ^ Data tag bit
  , memRespIsFinal :: Bit 1
    -- ^ Is this the final response of an atomic transaction?
    -- See note [Memory transactions]
  } deriving (Generic, Interface, Bits)

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

-- Decoding atomic operations
-- ==========================

-- | Decoded atomic operation
data DecodedAtomicOp =
  DecodedAtomicOp {
    amo_isSwap     :: Bit 1
  , amo_isXor      :: Bit 1
  , amo_isAnd      :: Bit 1
  , amo_isAdd      :: Bit 1
  , amo_isOr       :: Bit 1
  , amo_isMin      :: Bit 1
  , amo_isMinMax   :: Bit 1
  , amo_isUnsigned :: Bit 1
  } deriving (Generic, Bits, FShow)

-- | Decode atomic operation
decodeAtomicOp :: AtomicOp -> DecodedAtomicOp
decodeAtomicOp amo =
  DecodedAtomicOp {
    amo_isSwap   = amo .==. amoSwapOp
  , amo_isXor    = amo .==. amoXorOp
  , amo_isAdd    = amo .==. amoAddOp
  , amo_isAnd    = amo .==. amoAndOp
  , amo_isOr     = amo .==. amoOrOp
  , amo_isMin    = orList [amo .==. op | op <- [amoMinOp, amoMinUOp]]
  , amo_isMinMax = orList
      [amo .==. op | op <- [amoMinOp, amoMaxOp, amoMinUOp, amoMaxUOp]]
  , amo_isUnsigned = orList [amo .==. op | op <- [amoMinUOp, amoMaxUOp]]
  }

-- Note [Memory transactions]
-- ==========================

-- Memory transactions are sequences of requests (or responses) that
-- cannot be interleaved with other memory requests (or responses).
-- The scope of a transaction is simply defined using the
-- 'memReqIsFinal' bit (or the `memRespIsFinal` bit).  The main intent
-- here is to support accessing data items larger than 32 bits in an
-- atomic manner.  The mechanism can be misused to hog busses for long
-- durations, so use with care!

-- Memory requests from a CHERI-enabled processor
-- ==============================================

-- | A memory request able to represent a standard (32-bit) memory
-- request or a capability (64 bit) memory request.
data CapMemReq =
  CapMemReq {
    capMemReqStd :: MemReq
    -- ^ Standard memory request
  , capMemReqIsCapAccess :: Bit 1
    -- ^ Are we accessing a capability?
  , capMemReqUpperData :: Bit 32
    -- ^ Extra data field for capability store
  , capMemReqAbort :: Bit 1
    -- ^ Abort memory request?
    -- (Used to improve Fmax of complex CHERI exception conditions)
  } deriving (Generic, Interface, Bits)

-- | Convert 'MemReq's to 'CapMemReq's, ignoring capability meta-data
toCapMemReq :: MemReq -> CapMemReq
toCapMemReq req =
  CapMemReq {
    capMemReqStd = req
  , capMemReqIsCapAccess = false
  , capMemReqUpperData = dontCare
  , capMemReqAbort = false
  }
