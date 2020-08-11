module Pebbles where

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.Option
import Blarney.BitScan

-- Pebbles imports
import CSR
import Trap
import DataMem
import Pipeline

-- RISC-V I Decode
-- ===============

decodeI =
  [ "imm[31:12] rd<5> 0110111" --> "LUI"
  , "imm[31:12] rd<5> 0010111" --> "AUIPC"
  , "imm[11:0] rs1<5> 000 rd<5> 0010011" --> "ADD"
  , "imm[11:0] rs1<5> 010 rd<5> 0010011" --> "SLT"
  , "imm[11:0] rs1<5> 011 rd<5> 0010011" --> "SLTU"
  , "imm[11:0] rs1<5> 111 rd<5> 0010011" --> "AND"
  , "imm[11:0] rs1<5> 110 rd<5> 0010011" --> "OR"
  , "imm[11:0] rs1<5> 100 rd<5> 0010011" --> "XOR"
  , "0000000 imm[4:0] rs1<5> 001 rd<5> 0010011" --> "SLL"
  , "0000000 imm[4:0] rs1<5> 101 rd<5> 0010011" --> "SRL"
  , "0100000 imm[4:0] rs1<5> 101 rd<5> 0010011" --> "SRA"
  , "0000000 rs2<5> rs1<5> 000 rd<5> 0110011" --> "ADD"
  , "0000000 rs2<5> rs1<5> 010 rd<5> 0110011" --> "SLT"
  , "0000000 rs2<5> rs1<5> 011 rd<5> 0110011" --> "SLTU"
  , "0000000 rs2<5> rs1<5> 111 rd<5> 0110011" --> "AND"
  , "0000000 rs2<5> rs1<5> 110 rd<5> 0110011" --> "OR"
  , "0000000 rs2<5> rs1<5> 100 rd<5> 0110011" --> "XOR"
  , "0100000 rs2<5> rs1<5> 000 rd<5> 0110011" --> "SUB"
  , "0000000 rs2<5> rs1<5> 001 rd<5> 0110011" --> "SLL"
  , "0000000 rs2<5> rs1<5> 101 rd<5> 0110011" --> "SRL"
  , "0100000 rs2<5> rs1<5> 101 rd<5> 0110011" --> "SRA"
  , "imm[20] imm[10:1] imm[11] imm[19:12] rd<5> 1101111" --> "JAL"
  , "imm[11:0] rs1<5> 000 rd<5> 1100111" --> "JALR"
  , "off[12] off[10:5] rs2<5> rs1<5> 000 off[4:1] off[11] 1100011" --> "BEQ"
  , "off[12] off[10:5] rs2<5> rs1<5> 001 off[4:1] off[11] 1100011" --> "BNE"
  , "off[12] off[10:5] rs2<5> rs1<5> 100 off[4:1] off[11] 1100011" --> "BLT"
  , "off[12] off[10:5] rs2<5> rs1<5> 110 off[4:1] off[11] 1100011" --> "BLTU"
  , "off[12] off[10:5] rs2<5> rs1<5> 101 off[4:1] off[11] 1100011" --> "BGE"
  , "off[12] off[10:5] rs2<5> rs1<5> 111 off[4:1] off[11] 1100011" --> "BGEU"
  , "imm[11:0] rs1<5> ul<1> aw<2> rd<5> 0000011" --> "LOAD"
  , "imm[11:5] rs2<5> rs1<5> 0 aw<2> imm[4:0] 0100011" --> "STORE"
  , "<4> <4> <4> <5> 000 <5> 0001111" --> "FENCE"
  , "000000000000 <5> 000 <5> 1110011" --> "ECALL"
  , "000000000001 <5> 000 <5> 1110011" --> "EBREAK"
  , "imm[11:0] rs1<5> 001 rd<5> 1110011" --> "CSRRW"
  ]

-- RISC-V I Execute
-- ================

executeI :: CSRUnit -> State -> Action ()
executeI csrUnit s = do
  -- 33-bit add/sub/compare
  let uns = s.opcode `is` ["SLTU", "BLTU", "BGEU"]
  let addA = (uns ? (0, at @31 (s.opA))) # s.opA
  let addB = (uns ? (0, at @31 (s.opBorImm))) # s.opBorImm
  let isAdd = s.opcode `is` ["ADD"]
  let sum = addA + (isAdd ? (addB, inv addB))
                 + (isAdd ? (0, 1))
  let less = at @32 sum
  let equal = s.opA .==. s.opBorImm

  when (s.opcode `is` ["ADD", "SUB"]) do
    s.result <== truncate sum

  when (s.opcode `is` ["SLT", "SLTU"]) do
    s.result <== zeroExtend less

  when (s.opcode `is` ["AND"]) do
    s.result <== s.opA .&. s.opBorImm

  when (s.opcode `is` ["OR"]) do
    s.result <== s.opA .|. s.opBorImm

  when (s.opcode `is` ["XOR"]) do
    s.result <== s.opA .^. s.opBorImm

  when (s.opcode `is` ["LUI"]) do
    s.result <== s.opBorImm

  when (s.opcode `is` ["AUIPC"]) do
    s.result <== s.pc.val + s.opBorImm

  when (s.opcode `is` ["SLL"]) do
    s.result <== s.opA .<<. slice @4 @0 (s.opBorImm)

  when (s.opcode `is` ["SRL", "SRA"]) do
    let ext = s.opcode `is` ["SRA"] ? (at @31 (s.opA), 0)
    let opAExt = ext # (s.opA)
    s.result <== truncate (opAExt .>>>. slice @4 @0 (s.opBorImm))

  let branch =
        orList [
          s.opcode `is` ["BEQ"] .&. equal
        , s.opcode `is` ["BNE"] .&. inv equal
        , s.opcode `is` ["BLT", "BLTU"] .&. less
        , s.opcode `is` ["BGE", "BGEU"] .&. inv less
        ]

  when branch do
    let offset = getField (s.fields) "off"
    s.pc <== s.pc.val + offset.val

  when (s.opcode `is` ["JAL"]) do
    s.pc <== s.pc.val + s.opBorImm

  when (s.opcode `is` ["JALR"]) do
    s.pc <== truncateLSB (s.opA + s.opBorImm) # (0 :: Bit 1)

  when (s.opcode `is` ["JAL", "JALR"]) do
    s.result <== s.pc.val + 4

  -- Memory access
  let memAddr = s.opA + s.opBorImm
  let memAccessWidth = getField (s.fields) "aw"

  when (s.opcode `is` ["LOAD"]) do
    memLoad s memAddr

  when (s.opcode `is` ["STORE"]) do
    let byteEn = genByteEnable (memAccessWidth.val) memAddr
    memStore s memAddr byteEn (writeAlign (memAccessWidth.val) (s.opB))

  when (s.opcode `is` ["FENCE"]) do
    noAction

  when (s.opcode `is` ["ECALL"]) do
    trap s csrUnit (Exception exc_eCallFromU)

  when (s.opcode `is` ["EBREAK"]) do
    trap s csrUnit (Exception exc_breakpoint)

  when (s.opcode `is` ["CSRRW"]) do
    readCSR csrUnit (s.opBorImm.truncate) (s.result)
    writeCSR csrUnit (s.opBorImm.truncate) (s.opA)

-- RISC-V I memory access helpers
-- ==============================

-- RV32I memory access width
type AccessWidth = Bit 2

-- Byte, half-word, or word access?
isByteAccess, isHalfAccess, isWordAccess :: AccessWidth -> Bit 1
isByteAccess = (.==. 0b00)
isHalfAccess = (.==. 0b01)
isWordAccess = (.==. 0b10)

-- Determine byte enables given access-width and address
genByteEnable :: AccessWidth -> Bit 32 -> Bit 4
genByteEnable w addr =
  select [
    isWordAccess w --> 0b1111
  , isHalfAccess w --> (a.==.2) # (a.==.2) # (a.==.0) # (a.==.0)
  , isByteAccess w --> (a.==.3) # (a.==.2) # (a.==.1) # (a.==.0)
  ]
  where a :: Bit 2 = truncate addr

-- Align write-data using access-width
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

-- Determine result of load from memory response
loadMux :: Bit 32 -> Bit 32 -> AccessWidth -> Bit 1 -> Bit 32
loadMux respData addr w isUnsigned =
    select [
      isWordAccess w --> b3 # b2 # b1 # b0
    , isHalfAccess w --> hExt # h
    , isByteAccess w --> bExt # b
    ]
  where
    a = lower addr :: Bit 2
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

-- RISC-V I memory response
-- ========================

memResponseI :: State -> Bit 32 -> Action ()
memResponseI s respData = do
  let isUnsignedLoad = getField (s.fields) "ul"
  let memAccessWidth = getField (s.fields) "aw"
  s.result <== loadMux respData (s.opA + s.opBorImm)
                 (memAccessWidth.val) (isUnsignedLoad.val)

-- RISC-V M extension
-- ==================

-- TODO: implement division instructions too

decodeM =
  [ "0000001 rs2<5> rs1<5> 0 mul<2> rd<5> 0110011" --> "MUL"
  ]

-- Request to multiplier unit
data MulReq =
  MulReq {
    -- Unique identifier from pipeline
    mulReqId :: InstrId
    -- Operands to multiply
  , mulReqA :: Bit 32
  , mulReqB :: Bit 32
    -- Do we want the lower (or upper) bits of the result?
  , mulReqLower :: Bit 1
    -- Are the operands signed or unsigned?
  , mulReqUnsignedA :: Bit 1
  , mulReqUnsignedB :: Bit 1
  } deriving (Generic, Bits)

-- Multiplier unit interface
data MulUnit =
  MulUnit {
    canMul :: Bit 1
  , enqMul :: MulReq -> Action ()
  , resumeMul :: Stream ResumeReq
  }

-- Multiplier unit (half throughput to save area)
-- To get a DSP multiplier, inputs and outputs must be registered
-- We assume the two input operands are already registered
makeMulUnit :: Module MulUnit
makeMulUnit = do
  -- Registers for request and result
  reqReg <- makeReg dontCare
  resultReg <- makeReg dontCare

  -- Do we have space to store the result?
  fullReg <- makeReg false

  return
    MulUnit {
      canMul = fullReg.val.inv
    , enqMul = \req -> do
       let msbA = at @31 (req.mulReqA)
       let msbB = at @31 (req.mulReqB)
       let extA = req.mulReqUnsignedA ? (0, msbA)
       let extB = req.mulReqUnsignedB ? (0, msbB)
       let mulA = extA # req.mulReqA
       let mulB = extB # req.mulReqB
       reqReg <== req
       resultReg <== slice @63 @0 (fullMul True mulA mulB)
       fullReg <== true
    , resumeMul =
        Source {
          peek = 
            ResumeReq {
              resumeReqId = reqReg.val.mulReqId
            , resumeReqData = reqReg.val.mulReqLower ?
                (resultReg.val.lower, resultReg.val.upper)
            }
        , canPeek = fullReg.val
        , consume = fullReg <== false
        }
    }

-- Execute state for M extension
executeM :: MulUnit -> State -> Action ()
executeM mulUnit s = do
    when (s.opcode `is` ["MUL"]) do
      if mulUnit.canMul
        then do
          id <- s.suspend
          let mulInfo :: Option (Bit 2) = getField (s.fields) "mul"
          enqMul mulUnit
            MulReq {
              mulReqId = id
            , mulReqA = s.opA
            , mulReqB = s.opB
            , mulReqLower = mulInfo.val .==. 0b00
            , mulReqUnsignedA = mulInfo.val .==. 0b11
            , mulReqUnsignedB = at @1 (mulInfo.val)
            }
        else s.retry

-- RV32I core with UART input and output channels
-- ==============================================

makePebbles :: Bool -> Stream (Bit 8) -> Module (Stream (Bit 8))
makePebbles sim uartIn = mdo
  -- CSR unit
  (uartOut, csrUnit) <- makeCSRUnit uartIn

  -- Data tightly-coupled memory
  memResps <- makeDTCM sim memReqs

  -- Multiplier
  mulUnit <- makeMulUnit

  -- Processor pipeline
  let config =
        Config {
          decodeStage = decodeI ++ decodeM
        , executeStage = \s -> do
            executeI csrUnit s
            executeM mulUnit s
        , memResponseStage = memResponseI
        , resumeStage = mulUnit.resumeMul
        }
  memReqs <- makePipeline sim config memResps

  return uartOut
