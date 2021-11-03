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
                let tag = isValid .&&. resp.memRespId.instrTagMask
                let meta = tag # resp.memRespData
                let addr = dataReg.val
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

-- Memory requests from a CHERI-enabled processor
-- ==============================================

-- | A memory request able to represent a standard (32-bit) memory
-- request or a capability (64 bit) memory request.
data CapMemReq t_id =
  CapMemReq {
    capMemReqStd :: MemReq t_id
    -- ^ Standard memory request
  , capMemReqIsCapAccess :: Bit 1
    -- ^ Are we accessing a capability?
  , capMemReqUpperData :: Bit 32
    -- ^ Extra data field for capability store
  } deriving (Generic, Interface, Bits)

-- | Convert 'CapMemReq's to 'MemReq's by serialisation
makeCapMemReqSink :: Bits t_id =>
     Sink (MemReq t_id)
     -- ^ Sink for standard memory requets
  -> Module (Sink (CapMemReq t_id))
     -- ^ Sink for capability memory requests
makeCapMemReqSink memReqSink = do
  -- Are we currently serialising a request?
  busy :: Reg (Bit 1) <- makeReg false

  -- Register for request currently being serialised
  reqReg :: Reg (CapMemReq t_id) <- makeReg dontCare

  always do
    when (busy.val .&&. memReqSink.canPut) do
      let req = reqReg.val
      let stdReq = req.capMemReqStd
      -- Put the second (final) flit of the serialised request
      put memReqSink
        stdReq {
          memReqData = req.capMemReqUpperData
        , memReqAddr = stdReq.memReqAddr + 4
        , memReqIsFinal = true
        }
      -- Serialisation complete
      busy <== false

  return
    Sink {
      canPut = inv busy.val .&&. memReqSink.canPut
    , put = \req -> do
        reqReg <== req
        let stdReq = req.capMemReqStd
        -- A capability access will require serialisation
        busy <== req.capMemReqIsCapAccess
        -- Put the first flit of the serialised request
        put memReqSink
          stdReq {
            -- If it's a capability access, disable final bit
            memReqIsFinal = inv req.capMemReqIsCapAccess
          }
    }

-- | Convert 'MemReq's to 'CapMemReq's
toCapMemReq :: Bits t_id => MemReq t_id -> CapMemReq t_id
toCapMemReq req =
  CapMemReq {
    capMemReqStd = req
  , capMemReqIsCapAccess = false
  , capMemReqUpperData = dontCare
  }
