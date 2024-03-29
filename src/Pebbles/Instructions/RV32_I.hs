module Pebbles.Instructions.RV32_I
  ( decodeI
  , executeI
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
  , "imm[31:12] rd<5> 0010111" --> AUIPC
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
  , "imm[12] imm[10:5] rs2<5> rs1<5> 000 imm[4:1] imm[11] 1100011" --> BEQ
  , "imm[12] imm[10:5] rs2<5> rs1<5> 001 imm[4:1] imm[11] 1100011" --> BNE
  , "imm[12] imm[10:5] rs2<5> rs1<5> 100 imm[4:1] imm[11] 1100011" --> BLT
  , "imm[12] imm[10:5] rs2<5> rs1<5> 110 imm[4:1] imm[11] 1100011" --> BLTU
  , "imm[12] imm[10:5] rs2<5> rs1<5> 101 imm[4:1] imm[11] 1100011" --> BGE
  , "imm[12] imm[10:5] rs2<5> rs1<5> 111 imm[4:1] imm[11] 1100011" --> BGEU
  , "000000000000 <5> 000 <5> 1110011" --> ECALL
  , "000000000001 <5> 000 <5> 1110011" --> EBREAK
  , "fence<4> pred<4> succ<4> rs1<5> 000 00000 0001111" --> FENCE
  , "csrImm[11:0] rs1<5> csrI<1> 01 rd<5> 1110011" --> CSRRW
  , "csrImm[11:0] rs1<5> csrI<1> 10 rd<5> 1110011" --> CSRRS
  , "csrImm[11:0] rs1<5> csrI<1> 11 rd<5> 1110011" --> CSRRC
  , "imm[11:0] rs1<5> ul<1> aw<2> rd<5> 0000011" --> LOAD
  , "imm[11:5] rs2<5> rs1<5> 0 aw<2> imm[4:0] 0100011" --> STORE
  ]

-- Field selectors
-- ===============

getCSRI :: Bit 32 -> Bit 1
getCSRI = makeFieldSelector decodeI "csrI"

getCSRImm :: Bit 32 -> Bit 12
getCSRImm = makeFieldSelector decodeI "csrImm"

getFenceFlags :: Bit 32 -> Bit 4
getFenceFlags = makeFieldSelector decodeI "fence"

getAccessWidth :: Bit 32 -> Bit 2
getAccessWidth = makeFieldSelector decodeI "aw"

getIsUnsignedLoad :: Bit 32 -> Bit 1
getIsUnsignedLoad = makeFieldSelector decodeI "ul"

-- Execute stage
-- =============

executeI ::
     Maybe (Sink MulReq)
     -- ^ Optionally use multiplier to implement shifts
  -> Maybe CSRUnit
     -- ^ Access to CSRs
  -> Maybe (Sink MemReq)
     -- ^ Access to memory
  -> State
     -- ^ Pipeline state
  -> Action ()
executeI m_shiftUnit m_csrUnit m_memReqs s = do
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
    s.result <== s.opA .&. s.immOrOpB

  when (s.opcode `is` [OR]) do
    s.result <== s.opA .|. s.immOrOpB

  when (s.opcode `is` [XOR]) do
    s.result <== s.opA .^. s.immOrOpB

  when (s.opcode `is` [LUI]) do
    s.result <== s.immOrOpB

  case m_shiftUnit of

    -- Use barrel shifter
    Nothing -> do
      let shiftAmount = slice @4 @0 (s.immOrOpB)

      when (s.opcode `is` [SLL]) do
        s.result <== s.opA .<<. shiftAmount

      when (s.opcode `is` [SRL, SRA]) do
        let ext = s.opcode `is` [SRA] ? (at @31 (s.opA), 0)
        let opAExt = ext # (s.opA)
        s.result <== truncate (opAExt .>>>. shiftAmount)

    -- Use multiplier
    Just mulUnit -> do
      when (s.opcode `is` [SLL, SRL, SRA]) do
        if mulUnit.canPut
          then do
            s.suspend
            let shiftAmount = slice @4 @0 (s.immOrOpB)
            let noShift = shiftAmount .==. 0
            mulUnit.put
              MulReq {
                mulReqA = s.opA
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

  let pcNew = s.pc.val + s.immOrOpB
  when branch do
    s.pc <== pcNew

  when (s.opcode `is` [AUIPC]) do
    s.result <== pcNew

  when (s.opcode `is` [JAL]) do
    s.pc <== pcNew

  let plus = s.opA + s.immOrOpB
  when (s.opcode `is` [JALR]) do
    s.pc <== truncateLSB plus # (0 :: Bit 1)

  when (s.opcode `is` [JAL, JALR]) do
    s.result <== s.pc.val + 4

  when (s.opcode `is` [ECALL]) do
    trap s exc_eCallFromM

  when (s.opcode `is` [EBREAK]) do
    trap s exc_breakpoint

  -- CSRs
  case m_csrUnit of
    Nothing -> return ()
    Just csrUnit ->
      -- Control/status registers
      when (s.opcode `is` [CSRRW, CSRRS, CSRRC]) do
        -- Condition for reading CSR
        let doRead = s.opcode `is` [CSRRW] ? (s.resultIndex .!=. 0, true)
        -- Read CSR
        x <- whenAction doRead do csrUnitRead csrUnit (getCSRImm s.instr)
        s.result <== x
        -- Condition for writing CSR
        let doWrite = s.opcode `is` [CSRRS, CSRRC] ? (s.opAIndex .!=. 0, true)
        -- Determine operand
        let operand = getCSRI s.instr ? (zeroExtend s.opAIndex, s.opA)
        -- Data to write for CSRRS/CSRRC
        let maskedData = fromBitList
              [ cond ? (s.opcode `is` [CSRRS], old)
              | (old, cond) <- zip (toBitList x) (toBitList operand) ]
        -- Data to write
        let writeData = s.opcode `is` [CSRRW] ? (operand, maskedData)
        -- Write CSR
        when doWrite do csrUnitWrite csrUnit (getCSRImm s.instr) writeData

  -- Memory
  case m_memReqs of
    Nothing -> return ()
    Just memReqs -> do
      -- Memory fence
      when (s.opcode `is` [FENCE]) do
        if memReqs.canPut
          then do
            s.suspend
            -- Send request to memory unit
            put memReqs
              MemReq {
                memReqAccessWidth = dontCare
              , memReqOp = memGlobalFenceOp
              , memReqAMOInfo = dontCare
              , memReqAddr = dontCare
              , memReqData = dontCare
              , memReqDataTagBit = 0
              , memReqDataTagBitMask = 0
              , memReqIsUnsigned = dontCare
              , memReqIsFinal = true
              }
          else s.retry

      -- Memory access
      when (s.opcode `is` [LOAD, STORE]) do
        if memReqs.canPut
          then do
            -- Currently the memory subsystem doesn't issue store responses
            -- so we make sure to only suspend on a load
            let hasResp = s.opcode `is` [LOAD]
            when hasResp do s.suspend
            -- Send request to memory unit
            put memReqs
              MemReq {
                memReqAccessWidth = getAccessWidth s.instr
              , memReqOp =
                  if s.opcode `is` [LOAD] then memLoadOp else memStoreOp
              , memReqAMOInfo = dontCare
              , memReqAddr = plus
              , memReqData = s.opB
              , memReqDataTagBit = 0
              , memReqDataTagBitMask = 0
              , memReqIsUnsigned = getIsUnsignedLoad s.instr
              , memReqIsFinal = true
              }
          else s.retry
