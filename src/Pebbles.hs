module Pebbles where

-- Blarney imports
import Blarney
import Blarney.Stmt
import Blarney.Queue
import Blarney.Stream
import Blarney.Option
import Blarney.BitScan
import Blarney.SourceSink

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

executeI :: CSRUnit -> MemUnit -> State -> Action ()
executeI csrUnit memUnit s = do
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
  when (s.opcode `is` ["LOAD", "STORE"]) do
    let memAddr = s.opA + s.opBorImm
    let memAccessWidth = getField (s.fields) "aw"
    let memIsUnsignedLoad = getField (s.fields) "ul"
    if memUnit.memReqs.canPut
      then do
        let isStore = s.opcode `is` ["STORE"]
        -- Currently the memory subsystem doesn't issue store responses
        -- so we make sure to only suspend on a load
        id <- whenR (isStore.inv) (s.suspend)
        -- Send request to memory unit
        put (memUnit.memReqs)
          MemReq {
            memReqId          = id
          , memReqIsStore     = isStore
          , memReqAddr        = memAddr
          , memReqByteEn      = genByteEnable (memAccessWidth.val) memAddr
          , memReqData        = writeAlign (memAccessWidth.val) (s.opB)
          , memReqAccessWidth = memAccessWidth.val
          , memReqIsUnsigned  = memIsUnsignedLoad.val
          }
      else s.retry

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

memRespToResumeReq :: MemResp -> ResumeReq
memRespToResumeReq (respData, origReq) =
  ResumeReq {
    resumeReqId = origReq.memReqId
  , resumeReqData =
      loadMux respData (origReq.memReqAddr)
        (origReq.memReqAccessWidth) (origReq.memReqIsUnsigned)
  }

-- RISC-V M extension
-- ==================

decodeM =
  [ "0000001 rs2<5> rs1<5> 0 mul<2> rd<5> 0110011" --> "MUL"
  , "0000001 rs2<5> rs1<5> 1 div<2> rd<5> 0110011" --> "DIV"
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
    mulReqs :: Sink MulReq
  , mulResps :: Source ResumeReq
  }

-- Multiplier unit (half throughput to save area)
-- To get a DSP multiplier, inputs and outputs must be registered
-- We assume the two input operands are already registered
makeHalfMulUnit :: Module MulUnit
makeHalfMulUnit = do
  -- Registers for request and result
  reqReg <- makeReg dontCare
  resultReg <- makeReg dontCare

  -- Do we have space to store the result?
  fullReg <- makeReg false

  return
    MulUnit {
      mulReqs =
        Sink {
          canPut = fullReg.val.inv
        , put = \req -> do
            let msbA = at @31 (req.mulReqA)
            let msbB = at @31 (req.mulReqB)
            let extA = req.mulReqUnsignedA ? (0, msbA)
            let extB = req.mulReqUnsignedB ? (0, msbB)
            let mulA = extA # req.mulReqA
            let mulB = extB # req.mulReqB
            reqReg <== req
            resultReg <== slice @63 @0 (fullMul True mulA mulB)
            fullReg <== true
        }
    , mulResps =
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

-- Request to divider unit
data DivReq =
  DivReq {
    -- Unique identifier from pipeline
    divReqId :: InstrId
    -- Numerator and denominator
  , divReqNum :: Bit 32
  , divReqDenom :: Bit 32
    -- Signed or unsigned division?
  , divReqIsSigned :: Bit 1
    -- Do we want the quotient or remainder?
  , divReqGetRemainder :: Bit 1
  } deriving (Generic, Bits)

-- Divider unit interface
data DivUnit =
  DivUnit {
    divReqs :: Sink DivReq
  , divResps :: Source ResumeReq
  }

-- Divider unit (sequential state machine version)
makeSeqDivUnit :: Module DivUnit
makeSeqDivUnit = do
  -- Numerator, denominator, quotient, and remainder
  n :: Reg (Bit 32) <- makeReg dontCare
  d :: Reg (Bit 32) <- makeReg dontCare
  q :: Reg (Bit 32) <- makeReg dontCare
  r :: Reg (Bit 32) <- makeReg dontCare

  -- Counter
  count :: Reg (Bit 6) <- makeReg dontCare

  -- Is the result ready for consumption?
  done :: Reg (Bit 1) <- makeReg false

  -- Is the result currently being computed?
  busy :: Reg (Bit 1) <- makeReg false

  -- Trigger division state machine
  trigger :: Reg (Bit 1) <- makeDReg false

  -- Signed or unsigned division?
  isSigned :: Reg (Bit 1) <- makeReg dontCare

  -- Quotient or remainder?
  getRemainder :: Reg (Bit 1) <- makeReg dontCare

  -- Flip the sign of the result?
  negResult :: Reg (Bit 1) <- makeReg dontCare

  -- Output register
  output :: Reg (Bit 32) <- makeReg dontCare

  -- Remember the request id
  reqId :: Reg InstrId <- makeReg dontCare

  -- Helper function to shift left by one
  let shl x = x .<<. (1 :: Bit 1)

  -- State machine
  runStmtOn (trigger.val) do
    -- Prepare division
    action do
      let divByZero = d.val .==. 0
      let n_msb = at @31 (n.val)
      let d_msb = at @31 (d.val)
      -- Negate numerator and denominator if required
      when (isSigned.val .&. divByZero.inv) do
        when n_msb do
          n <== n.val.negate
        when d_msb do
          d <== d.val.negate
      -- Initialise state machine
      q <== 0
      r <== 0
      count <== 32
      -- Negate result?
      negResult <== isSigned.val .&. divByZero.inv .&.
        (getRemainder.val ? (n_msb, n_msb .^. d_msb))
    -- Binary long division algorithm (taken from Wikipedia)
    while (count.val .!=. 0) do
      action do
        let r' = r.val.shl .|. zeroExtend (at @31 (n.val))
        let sub = r' .>=. d.val
        r <== r' - (sub ? (d.val, 0))
        q <== q.val.shl .|. (sub ? (1, 0))
        n <== n.val.shl
        count <== count.val - 1
    -- Prepare result
    action do
      done <== true
      busy <== false
      -- Choose quotient or remainder, and optionally negate
      let result = getRemainder.val ? (r.val, q.val)
      output <== negResult.val ? (result.negate, result)

  return
    DivUnit {
      divReqs =
        Sink {
          canPut = busy.val.inv .&. done.val.inv
        , put = \req -> do
            reqId <== req.divReqId
            n <== req.divReqNum
            d <== req.divReqDenom
            isSigned <== req.divReqIsSigned
            getRemainder <== req.divReqGetRemainder
            busy <== true
            trigger <== true
        }
    , divResps =
        Source {
          canPeek = done.val
        , peek =
            ResumeReq {
              resumeReqId = reqId.val
            , resumeReqData = output.val
            }
        , consume = do done <== false
        }
    }

-- Execute state for M extension
executeM :: MulUnit -> DivUnit -> State -> Action ()
executeM mulUnit divUnit s = do
  when (s.opcode `is` ["MUL"]) do
    if mulUnit.mulReqs.canPut
      then do
        id <- s.suspend
        let mulInfo :: Option (Bit 2) = getField (s.fields) "mul"
        put (mulUnit.mulReqs)
          MulReq {
            mulReqId = id
          , mulReqA = s.opA
          , mulReqB = s.opB
          , mulReqLower = mulInfo.val .==. 0b00
          , mulReqUnsignedA = mulInfo.val .==. 0b11
          , mulReqUnsignedB = at @1 (mulInfo.val)
          }
      else s.retry

  when (s.opcode `is` ["DIV"]) do
    if mulUnit.mulReqs.canPut
      then do
        id <- s.suspend
        let divInfo :: Option (Bit 2) = getField (s.fields) "div"
        put (divUnit.divReqs)
          DivReq {
            divReqId = id
          , divReqNum = s.opA
          , divReqDenom = s.opB
          , divReqIsSigned = at @0 (divInfo.val.inv)
          , divReqGetRemainder = at @1 (divInfo.val)
          }
      else s.retry

-- RV32I core with UART input and output channels
-- ==============================================

makePebbles :: Bool -> Stream (Bit 8) -> Module (Stream (Bit 8))
makePebbles sim uartIn = mdo
  -- CSR unit
  (uartOut, csrUnit) <- makeCSRUnit uartIn

  -- Tightly-coupled data memory
  memUnit <- makeDTCM sim

  -- Multiplier
  mulUnit <- makeHalfMulUnit

  -- Divider
  divUnit <- makeSeqDivUnit

  -- Processor pipeline
  makePipeline sim 
    Config {
      decodeStage = decodeI ++ decodeM
    , executeStage = \s -> do
        executeI csrUnit memUnit s
        executeM mulUnit divUnit s
    , resumeStage = mergeSourcesTree
        [ fmap memRespToResumeReq (memUnit.memResps)
        , mulUnit.mulResps
        , divUnit.divResps
        ]
    }

  return uartOut
