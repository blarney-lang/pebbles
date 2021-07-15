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
  , memRespIsFinal :: Bit 1
    -- ^ Is this the final response of an atomic transaction?
    -- See note [Memory transactions]
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

-- Note [Memory transactions]
-- ==========================

-- Memory transactions are sequences of requests (or responses) that
-- cannot be interleaved with other memory requests (or responses).
-- The scope of a transaction is simply defined using the
-- 'memReqIsFinal' bit (or the `memRespIsFinal` bit).  The main intent
-- here is to support accessing data items larger than 32 bits in an
-- atomic manner.  The mechanism can be misused to hog busses for long
-- durations, so use with care!

-- Resume request helpers
-- ======================

-- | Convert memory response flit(s) to pipeline resume request
makeMemRespToResumeReq ::
     Bool
     -- ^ Is CHERI enabled?
  -> Stream (MemResp InstrInfo)
     -- ^ Stream of memory response flits
  -> Module (Stream ResumeReq)
     -- ^ Stream of resume requests
makeMemRespToResumeReq enableCHERI memResps
  | not enableCHERI =
      return $
        memResps {
          peek =
            ResumeReq {
              resumeReqInfo = memResps.peek.memRespId
            , resumeReqData = memResps.peek.memRespData
            , resumeReqCap = none
           }
        }
  | otherwise = do
      -- Output buffer
      outQueue :: Queue ResumeReq <- makePipelineQueue 1

      -- Count flits in response
      flitCount :: Reg (Bit 1) <- makeReg 0

      -- Tag bit accumulator
      tagBitReg :: Reg (Bit 1) <- makeReg true

      -- Data acummulator
      dataReg :: Reg (Bit 32) <- makeReg dontCare

      always do
        when (memResps.canPeek) do
          let resp = memResps.peek

          -- Currently, only one or two response flits are expected
          -- Two-flit responses are assumed to be capabilities
          dynamicAssert (flitCount.val .!=. 0 .==>. resp.memRespIsFinal)
            "makeMemRespToResumeReq: too many flits in memory response"

          -- The capability is valid if all its tag bits are set
          let isValid = tagBitReg.val .&&. resp.memRespDataTagBit

          if resp.memRespIsFinal
            then do
              when (outQueue.notFull) do
                -- Reset accumlators for next transaction
                flitCount <== 0
                tagBitReg <== true
                -- Decode capability
                let cap = fromMem (isValid, resp.memRespData # dataReg.val)
                -- Split capability into address and meta-data
                let (meta, addr) = split cap
                -- Create resume request
                enq outQueue
                  ResumeReq {
                    resumeReqInfo = resp.memRespId
                  , resumeReqData =
                      if flitCount.val .==. 1 then addr else resp.memRespData
                  , resumeReqCap = Option (flitCount.val .==. 1) meta
                  }
                -- Consume flit
                memResps.consume
            else do
              -- Accumulate response
              tagBitReg <== isValid
              dataReg <== resp.memRespData
              flitCount <== flitCount.val + 1
              -- Consume flit
              memResps.consume

      return (toStream outQueue)
