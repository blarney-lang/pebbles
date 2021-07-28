module Pebbles.Instructions.RV32_I
  ( decodeI
  , decodeI_NoCap
  , executeI
  , executeI_NoCap
  ) where

-- Blarney imports
import Blarney
import Blarney.Stmt
import Blarney.Queue
import Blarney.Stream
import Blarney.BitScan
import Blarney.SourceSink

-- Pebbles imports
import Pebbles.CSRs.CSRUnit
import Pebbles.CSRs.TrapCodes
import Pebbles.Memory.Interface
import Pebbles.Pipeline.Interface
import Pebbles.Instructions.Mnemonics
import Pebbles.Instructions.Units.AddUnit
import Pebbles.Instructions.Units.MulUnit

-- Decode stage
-- ============

decodeI =
  [ "imm[31:12] rd<5> 0110111" --> LUI
  , "imm[11:0] rs1<5> 000 rd<5> 0010011" --> ADD
  , "imm[11:0] rs1<5> 010 rd<5> 0010011" --> SLT
  , "imm[11:0] rs1<5> 011 rd<5> 0010011" --> SLTU
  , "imm[11:0] rs1<5> 111 rd<5> 0010011" --> AND
  , "imm[11:0] rs1<5> 110 rd<5> 0010011" --> OR
  , "imm[11:0] rs1<5> 100 rd<5> 0010011" --> XOR
  , "0000000 imm[4:0] rs1<5> 001 rd<5> 0010011" --> SLL
  , "0000000 imm[4:0] rs1<5> 101 rd<5> 0010011" --> SRL
  , "0100000 imm[4:0] rs1<5> 101 rd<5> 0010011" --> SRA
  , "0000000 rs2<5> rs1<5> 000 rd<5> 0110011" --> ADD
  , "0000000 rs2<5> rs1<5> 010 rd<5> 0110011" --> SLT
  , "0000000 rs2<5> rs1<5> 011 rd<5> 0110011" --> SLTU
  , "0000000 rs2<5> rs1<5> 111 rd<5> 0110011" --> AND
  , "0000000 rs2<5> rs1<5> 110 rd<5> 0110011" --> OR
  , "0000000 rs2<5> rs1<5> 100 rd<5> 0110011" --> XOR
  , "0100000 rs2<5> rs1<5> 000 rd<5> 0110011" --> SUB
  , "0000000 rs2<5> rs1<5> 001 rd<5> 0110011" --> SLL
  , "0000000 rs2<5> rs1<5> 101 rd<5> 0110011" --> SRL
  , "0100000 rs2<5> rs1<5> 101 rd<5> 0110011" --> SRA
  , "imm[20] imm[10:1] imm[11] imm[19:12] rd<5> 1101111" --> JAL
  , "imm[11:0] rs1<5> 000 rd<5> 1100111" --> JALR
  , "off[12] off[10:5] rs2<5> rs1<5> 000 off[4:1] off[11] 1100011" --> BEQ
  , "off[12] off[10:5] rs2<5> rs1<5> 001 off[4:1] off[11] 1100011" --> BNE
  , "off[12] off[10:5] rs2<5> rs1<5> 100 off[4:1] off[11] 1100011" --> BLT
  , "off[12] off[10:5] rs2<5> rs1<5> 110 off[4:1] off[11] 1100011" --> BLTU
  , "off[12] off[10:5] rs2<5> rs1<5> 101 off[4:1] off[11] 1100011" --> BGE
  , "off[12] off[10:5] rs2<5> rs1<5> 111 off[4:1] off[11] 1100011" --> BGEU
  , "000000000000 <5> 000 <5> 1110011" --> ECALL
  , "000000000001 <5> 000 <5> 1110011" --> EBREAK
  , "fence<4> pred<4> succ<4> rs1<5> 000 00000 0001111" --> FENCE
  , "csrImm[11:0] rs1<5> csrI<1> 01 rd<5> 1110011" --> CSRRW
  , "csrImm[11:0] rs1<5> csrI<1> 10 rd<5> 1110011" --> CSRRS
  , "csrImm[11:0] rs1<5> csrI<1> 11 rd<5> 1110011" --> CSRRC
  ]

decodeI_NoCap =
  [ "imm[31:12] rd<5> 0010111" --> AUIPC
  , "imm[11:0] rs1<5> ul<1> aw<2> rd<5> 0000011" --> LOAD
  , "imm[11:5] rs2<5> rs1<5> 0 aw<2> imm[4:0] 0100011" --> STORE
  ]

-- Field selectors
-- ===============

getBranchOffset :: Bit 32 -> Bit 13
getBranchOffset = makeFieldSelector decodeI "off"

getCSRI :: Bit 32 -> Bit 1
getCSRI = makeFieldSelector decodeI "csrI"

getCSRImm :: Bit 32 -> Bit 12
getCSRImm = makeFieldSelector decodeI "csrImm"

getFenceFlags :: Bit 32 -> Bit 4
getFenceFlags = makeFieldSelector decodeI "fence"

getAccessWidth :: Bit 32 -> Bit 2
getAccessWidth = makeFieldSelector decodeI_NoCap "aw"

getIsUnsignedLoad :: Bit 32 -> Bit 1
getIsUnsignedLoad = makeFieldSelector decodeI_NoCap "ul"

-- Execute stage
-- =============

executeI ::
     Maybe MulUnit
     -- ^ Optionally use multiplier to implement shifts
  -> CSRUnit
     -- ^ Access to CSRs
  -> Sink (MemReq InstrInfo)
     -- ^ Access to memory
  -> State
     -- ^ Pipeline state
  -> Action ()
executeI shiftUnit csrUnit memReqs s = do
  -- Add/sub/compare unit
  let AddOuts sum less equal = addUnit 
        AddIns {
          addSub = inv (s.opcode `is` [ADD])
        , addUnsigned = s.opcode `is` [SLTU, BLTU, BGEU]
        , addOpA = s.opA
        , addOpB = s.opBorImm
        }

  when (s.opcode `is` [ADD, SUB]) do
    s.result <== truncate sum

  when (s.opcode `is` [SLT, SLTU]) do
    s.result <== zeroExtend less

  when (s.opcode `is` [AND]) do
    s.result <== s.opA .&. s.opBorImm

  when (s.opcode `is` [OR]) do
    s.result <== s.opA .|. s.opBorImm

  when (s.opcode `is` [XOR]) do
    s.result <== s.opA .^. s.opBorImm

  when (s.opcode `is` [LUI]) do
    s.result <== s.opBorImm

  case shiftUnit of

    -- Use barrel shifter
    Nothing -> do
      let shiftAmount = slice @4 @0 (s.opBorImm)

      when (s.opcode `is` [SLL]) do
        s.result <== s.opA .<<. shiftAmount

      when (s.opcode `is` [SRL, SRA]) do
        let ext = s.opcode `is` [SRA] ? (at @31 (s.opA), 0)
        let opAExt = ext # (s.opA)
        s.result <== truncate (opAExt .>>>. shiftAmount)

    -- Use multiplier
    Just mulUnit -> do
      when (s.opcode `is` [SLL, SRL, SRA]) do
        if mulUnit.mulReqs.canPut
          then do
            info <- s.suspend
            let shiftAmount = slice @4 @0 (s.opBorImm)
            let noShift = shiftAmount .==. 0
            put (mulUnit.mulReqs)
              MulReq {
                mulReqInfo = info
              , mulReqA = s.opA
              , mulReqB = 1 .<<. (if s.opcode `is` [SLL]
                  then shiftAmount else negate shiftAmount)
              , mulReqLower = s.opcode `is` [SLL] .||. noShift
              , mulReqUnsignedA = s.opcode `is` [SRL, SLL] .||. noShift
              , mulReqUnsignedB = true
              }
          else s.retry

  let branch =
        orList [
          s.opcode `is` [BEQ] .&. equal
        , s.opcode `is` [BNE] .&. inv equal
        , s.opcode `is` [BLT, BLTU] .&. less
        , s.opcode `is` [BGE, BGEU] .&. inv less
        ]

  when branch do
    s.pc <== s.pc.val + s.instr.getBranchOffset.signExtend

  when (s.opcode `is` [JAL]) do
    s.pc <== s.pc.val + s.opBorImm

  when (s.opcode `is` [JALR]) do
    s.pc <== truncateLSB (s.opA + s.opBorImm) # (0 :: Bit 1)

  when (s.opcode `is` [JAL, JALR]) do
    s.result <== s.pc.val + 4

  when (s.opcode `is` [ECALL]) do
    trap s exc_eCallFromM

  when (s.opcode `is` [EBREAK]) do
    trap s exc_breakpoint

  -- Memory fence
  when (s.opcode `is` [FENCE]) do
    if memReqs.canPut
      then do
        info <- s.suspend
        -- Send request to memory unit
        put memReqs
          MemReq {
            memReqId = info
          , memReqAccessWidth = dontCare
          , memReqOp =
              if at @0 (s.instr.getFenceFlags)
                then memLocalFenceOp else memGlobalFenceOp
          , memReqAMOInfo = dontCare
          , memReqAddr = dontCare
          , memReqData = dontCare
          , memReqDataTagBit = 0
          , memReqIsUnsigned = dontCare
          , memReqIsFinal = true
          }
      else s.retry

  -- Control/status registers
  when (s.opcode `is` [CSRRW, CSRRS, CSRRC]) do
    -- Condition for reading CSR
    let doRead = s.opcode `is` [CSRRW] ? (s.resultIndex .!=. 0, true)
    -- Read CSR
    x <- whenR doRead do csrUnitRead csrUnit (s.instr.getCSRImm)
    s.result <== x
    -- Condition for writing CSR
    let doWrite = s.opcode `is` [CSRRS, CSRRC] ? (s.opAIndex .!=. 0, true)
    -- Determine operand
    let operand = s.instr.getCSRI ? (s.opAIndex.zeroExtend, s.opA)
    -- Data to write for CSRRS/CSRRC
    let maskedData = fromBitList
          [ cond ? (s.opcode `is` [CSRRS], old)
          | (old, cond) <- zip (toBitList x) (toBitList operand) ]
    -- Data to write
    let writeData = s.opcode `is` [CSRRW] ? (operand, maskedData)
    -- Write CSR
    when doWrite do csrUnitWrite csrUnit (s.instr.getCSRImm) writeData

executeI_NoCap ::
     CSRUnit
     -- ^ Access to CSRs
  -> Sink (MemReq InstrInfo)
     -- ^ Access to memory
  -> State
     -- ^ Pipeline state
  -> Action ()
executeI_NoCap csrUnit memReqs s = do

  when (s.opcode `is` [AUIPC]) do
    s.result <== s.pc.val + s.opBorImm

  -- Memory access
  when (s.opcode `is` [LOAD, STORE]) do
    if memReqs.canPut
      then do
        -- Currently the memory subsystem doesn't issue store responses
        -- so we make sure to only suspend on a load
        let hasResp = s.opcode `is` [LOAD]
        info <- whenR hasResp (s.suspend)
        -- Send request to memory unit
        put memReqs
          MemReq {
            memReqId = info
          , memReqAccessWidth = s.instr.getAccessWidth
          , memReqOp =
              if s.opcode `is` [LOAD] then memLoadOp else memStoreOp
          , memReqAMOInfo = dontCare
          , memReqAddr = s.opA + s.opBorImm
          , memReqData = s.opB
          , memReqDataTagBit = 0
          , memReqIsUnsigned = s.instr.getIsUnsignedLoad
          , memReqIsFinal = true
          }
      else s.retry
