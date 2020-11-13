module Pebbles.Instructions.Memory where

-- Blarney imports
import Blarney

-- Pebbles imports
import Pebbles.Memory.Interface
import Pebbles.Pipeline.Interface

-- Memory access helpers
-- =====================

-- | RV32I memory access width (bytes = 2 ^ AccessWidth)
type AccessWidth = Bit 2

-- | Byte, half-word, or word access?
isByteAccess, isHalfAccess, isWordAccess :: AccessWidth -> Bit 1
isByteAccess = (.==. 0b00)
isHalfAccess = (.==. 0b01)
isWordAccess = (.==. 0b10)

-- | Determine byte enables given access-width and address
genByteEnable :: AccessWidth -> Bit 32 -> Bit 4
genByteEnable w addr =
  select [
    isWordAccess w --> 0b1111
  , isHalfAccess w --> (a.==.2) # (a.==.2) # (a.==.0) # (a.==.0)
  , isByteAccess w --> (a.==.3) # (a.==.2) # (a.==.1) # (a.==.0)
  ]
  where a :: Bit 2 = truncate addr

-- | Align write-data using access-width
writeAlign :: AccessWidth -> Bit 32 -> Bit 32
writeAlign w d =
  select [
    isWordAccess w --> b3 # b2 # b1 # b0
  , isHalfAccess w --> b1 # b0 # b1 # b0
  , isByteAccess w --> b0 # b0 # b0 # b0
  ]
  where
    b0 = slice @7 @0 d
    b1 = slice @15 @8 d
    b2 = slice @23 @16 d
    b3 = slice @31 @24 d

-- | Determine result of load from memory response
loadMux :: Bit 32 -> Bit 2 -> AccessWidth -> Bit 1 -> Bit 32
loadMux respData a w isUnsigned =
    select [
      isWordAccess w --> b3 # b2 # b1 # b0
    , isHalfAccess w --> hExt # h
    , isByteAccess w --> bExt # b
    ]
  where
    b = select [
          a .==. 0 --> b0
        , a .==. 1 --> b1
        , a .==. 2 --> b2
        , a .==. 3 --> b3
        ]
    h = (at @1 a .==. 0) ? (b1 # b0, b3 # b2)
    bExt = isUnsigned ? (0, signExtend (at @7 b))
    hExt = isUnsigned ? (0, signExtend (at @15 h))
    b0 = slice @7 @0 respData
    b1 = slice @15 @8 respData
    b2 = slice @23 @16 respData
    b3 = slice @31 @24 respData

-- | Memory request identifer, which we use to store information about
-- the request that's typically needed when processing the response
data MemReqId =
  MemReqId {
    -- | Access width of request
    memReqIdLogAccessWidth :: AccessWidth
    -- | Lower bits of the address
  , memReqIdAddr :: Bit 2
    -- | Is it an unsigned load?
  , memReqIdIsUnsigned :: Bit 1
    -- | Information about the suspended instruction that needs
    -- to be passed back to the pipeline in the resume stage
  , memReqIdInstrInfo :: InstrInfo
  } deriving (Generic, Bits)

-- | Convert memory response to pipeline resume request
memRespToResumeReq :: MemResp MemReqId -> ResumeReq
memRespToResumeReq resp =
  ResumeReq {
    resumeReqInfo = id.memReqIdInstrInfo
  , resumeReqData =
      loadMux (resp.memRespData) (id.memReqIdAddr)
        (id.memReqIdLogAccessWidth) (id.memReqIdIsUnsigned)
  }
  where id = resp.memRespId
