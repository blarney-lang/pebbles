-- Subset of CHERI (capability mode only)

module Pebbles.Instructions.RV32_IxCHERI where

-- Blarney imports
import Blarney
import Blarney.Stmt
import Blarney.Queue
import Blarney.Stream
import Blarney.BitScan
import Blarney.SourceSink
import Blarney.TaggedUnion hiding (is)

-- Pebbles imports
import Pebbles.CSRs.CSRUnit
import Pebbles.CSRs.TrapCodes
import Pebbles.Memory.Interface
import Pebbles.Pipeline.Interface
import Pebbles.Instructions.Mnemonics
import Pebbles.Instructions.Units.SFU
import Pebbles.Instructions.Units.AddUnit
import Pebbles.Instructions.Units.MulUnit
import Pebbles.Instructions.Units.BoundsUnit

-- CHERI imports
import CHERI.CapLib

-- Decode stage
-- ============

decodeIxCHERI =
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
  , "1111111 00000  rs1<5> 000 rd<5> 1011011" --> CGetPerm
  , "1111111 00001  rs1<5> 000 rd<5> 1011011" --> CGetType
  , "1111111 00010  rs1<5> 000 rd<5> 1011011" --> CGetBase
  , "1111111 00011  rs1<5> 000 rd<5> 1011011" --> CGetLen
  , "1111111 00100  rs1<5> 000 rd<5> 1011011" --> CGetTag
  , "1111111 00101  rs1<5> 000 rd<5> 1011011" --> CGetSealed
  , "1111111 00111  rs1<5> 000 rd<5> 1011011" --> CGetFlags
  , "1111111 01111  rs1<5> 000 rd<5> 1011011" --> CGetAddr
  , "0001101 rs2<5> rs1<5> 000 rd<5> 1011011" --> CAndPerm
  , "0001110 rs2<5> rs1<5> 000 rd<5> 1011011" --> CSetFlags
  , "0010000 rs2<5> rs1<5> 000 rd<5> 1011011" --> CSetAddr
  , "0010001 rs2<5> rs1<5> 000 rd<5> 1011011" --> CIncOffset
  , "imm[11:0]      rs1<5> 001 rd<5> 1011011" --> CIncOffset
  , "0001000 rs2<5> rs1<5> 000 rd<5> 1011011" --> CSetBounds
  , "uimm[11:0]     rs1<5> 010 rd<5> 1011011" --> CSetBoundsImm
  , "0001001 rs2<5> rs1<5> 000 rd<5> 1011011" --> CSetBoundsExact
  , "0010100 rs2<5> rs1<5> 000 rd<5> 1011011" --> SUB
  , "1111111 01010  rs1<5> 000 rd<5> 1011011" --> CMove
  , "1111111 01011  rs1<5> 000 rd<5> 1011011" --> CClearTag
  , "0000001 scr<5> rs1<5> 000 rd<5> 1011011" --> CSpecialRW
  , "1111111 10001  rs1<5> 000 rd<5> 1011011" --> CSealEntry
  , "1111111 01000  rs1<5> 000 rd<5> 1011011" --> CRRL
  , "1111111 01001  rs1<5> 000 rd<5> 1011011" --> CRAM
  ]

decodeAxCHERI =
  [ "imm<0> amo<5> aq<1> rl<1> rs2<5> rs1<5> 010 rd<5> 0101111" --> AMO
  ]

-- Field selectors
-- ===============

getCSRI :: Bit 32 -> Bit 1
getCSRI = makeFieldSelector decodeIxCHERI "csrI"

getCSRImm :: Bit 32 -> Bit 12
getCSRImm = makeFieldSelector decodeIxCHERI "csrImm"

getFenceFlags :: Bit 32 -> Bit 4
getFenceFlags = makeFieldSelector decodeIxCHERI "fence"

getAccessWidth :: Bit 32 -> Bit 2
getAccessWidth = makeFieldSelector decodeIxCHERI "aw"

getIsUnsignedLoad :: Bit 32 -> Bit 1
getIsUnsignedLoad = makeFieldSelector decodeIxCHERI "ul"

getAMO :: Bit 32 -> Bit 5
getAMO = makeFieldSelector decodeAxCHERI "amo"

getAcquire :: Bit 32 -> Bit 1
getAcquire = makeFieldSelector decodeAxCHERI "aq"

getRelease :: Bit 32 -> Bit 1
getRelease = makeFieldSelector decodeAxCHERI "rl"

getUImm :: Bit 32 -> Bit 12
getUImm = makeFieldSelector decodeIxCHERI "uimm"

-- Execute stage (no shared bounds unit)
-- =====================================

executeIxCHERI ::
     Maybe (Sink MulReq)
     -- ^ Optionally use multiplier to implement shifts
  -> Maybe CSRUnit
     -- ^ Access to CSRs
  -> Maybe (Sink CapMemReq)
     -- ^ Access to memory
  -> State
     -- ^ Pipeline state
  -> Action ()
executeIxCHERI m_shiftUnit m_csrUnit m_memReqs s = do
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

  when (s.opcode `is` [ECALL]) do
    trap s exc_eCallFromM

  when (s.opcode `is` [EBREAK]) do
    trap s exc_breakpoint

  -- Control/status registers
  case m_csrUnit of
    Nothing -> return ()
    Just csrUnit ->
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

  -- Shorthands / shared logic for capability operands
  let cA = s.capA.capPipe
  let cB = s.capB.capPipe
  let topA = s.capA.capTop
  let baseA = s.capA.capBase
  let lenA = s.capA.capLength
  let addrA = getAddr cA
  let permsA = getHardPerms cA
  let permsB = getHardPerms cB

  -- Capability inspection instructions
  -- ----------------------------------

  let isInspect = s.opcode `is`
        [ CGetPerm, CGetType, CGetTag, CGetBase, CGetLen
        , CGetSealed, CGetFlags, CGetAddr
        ]

  when isInspect do
    s.result <==
      select
        [ s.opcode `is` [CGetPerm] --> zeroExtend (getPerms cA)
        , s.opcode `is` [CGetType] -->
            let t = getType cA in
              if isSealedWithType cA then zeroExtend t else signExtend t
        , s.opcode `is` [CGetTag] --> zeroExtend (isValidCap cA)
        , s.opcode `is` [CGetSealed] --> zeroExtend (isSealed cA)
        , s.opcode `is` [CGetFlags] --> zeroExtend (getFlags cA)
        , s.opcode `is` [CGetAddr] --> addrA
        , s.opcode `is` [CGetBase] --> baseA
        , s.opcode `is` [CGetLen] -->
            if at @32 lenA then ones else lower lenA
        ]

  -- Special capability registers
  -- ----------------------------

  -- Non-compliant; always returns the almighty capability
  when (s.opcode `is` [CSpecialRW]) do
    s.resultCap <== almightyCapPipeVal

  -- Other capability modification instructions
  -- ------------------------------------------

  -- Exception paths
  when (s.opcode `is` [CSetFlags, CSetAddr, CIncOffset]) do
    when (isValidCap cA .&&. isSealed cA) do
      trap s cheri_exc_sealViolation

  when (s.opcode `is` [CSealEntry, CAndPerm]) do
    if inv (isValidCap cA) then
      trap s cheri_exc_tagViolation
    else if isSealed cA then
      trap s cheri_exc_sealViolation
    else if s.opcode `is` [CSealEntry] .&&. inv permsA.permitExecute then
      trap s cheri_exc_permitExecuteViolation
    else
      return ()

  when (s.opcode `is` [CMove, CSealEntry, CSetFlags, CClearTag, CAndPerm]) do
    let newType  = if s.opcode `is` [CSealEntry]
                     then -2
                     else getType cA
    let newValid = if s.opcode `is` [CClearTag]
                     then 0
                     else isValidCap cA
    let newFlags = if s.opcode `is` [CSetFlags]
                     then lower s.opB
                     else getFlags cA
    let newPerms = if s.opcode `is` [CAndPerm]
                     then getPerms cA .&. lower s.opB
                     else getPerms cA
    s.resultCap <==
      ( flip setValidCap newValid
      $ flip setFlags newFlags
      $ flip setType newType
      $ flip setPerms newPerms
      $ cA )

  -- Address-setting logic
  let oldCap = if s.opcode `is` [CSetAddr, CIncOffset, JALR]
                 then cA else s.pcc.capPipe
  let newAddr0 = s.immOrOpB +
        (if s.opcode `is` [CSetAddr] then 0 else getAddr oldCap)
  let newAddr1 = upper newAddr0 #
                   (if s.opcode `is` [JALR]
                      then 0 :: Bit 1 else lower newAddr0)
  let newCap = setAddr oldCap newAddr1

  when (s.opcode `is` [CSetAddr, CIncOffset, AUIPC]) do
    s.resultCap <== newCap.value

  -- Capability jumps/branches
  -- -------------------------

  when (s.opcode `is` [JAL, JALR]) do
    -- Exception path
    when (isSealed newCap.value .&&. inv (isSentry newCap.value)) do
      trap s cheri_exc_sealViolation

    -- Result path
    -- Use setAddrUnsafe: PC is in bounds therefore PC+4 is representable
    let linkCap = setAddrUnsafe s.pcc.capPipe (s.pc.val + 4)
    s.resultCap <== setType linkCap (-2) {- Seal as sentry -}

  when ((s.opcode `is` [JAL, JALR]) .||. branch) do
    let newType = if s.opcode `is` [JALR] then -1 else getType newCap.value
    s.pccNew <== setType newCap.value newType {- Unseal -}

  -- Bounds-setting instructions
  -- ---------------------------

  when (s.opcode `is` [CSetBounds, CSetBoundsImm, CSetBoundsExact]) do
    let b = if s.opcode `is` [CSetBoundsImm]
              then zeroExtend (getUImm s.instr)
              else s.opB
    let newCap = setBounds cA b
    let needExact = s.opcode `is` [CSetBoundsExact]

    -- Exception path
    if inv (isValidCap cA) then
      trap s cheri_exc_tagViolation
    else if isSealed cA then
      trap s cheri_exc_sealViolation
    else if addrA .<. baseA .||.
              zeroExtend addrA + zeroExtend b .>. topA then
      trap s cheri_exc_lengthViolation
    else if needExact .&&. inv newCap.exact then
      trap s cheri_exc_representabilityViolation
    else return ()

    -- Result path
    s.resultCap <== newCap.value

  when (s.opcode `is` [CRRL, CRAM]) do
    let result = setBoundsCombined nullCapPipe s.opA
    s.result <== s.opcode `is` [CRRL] ? (result.length, result.mask)


  -- Memory access
  -- -------------

  case m_memReqs of
    Nothing -> return ()
    Just memReqs -> do
      when (s.opcode `is` [LOAD, STORE, AMO]) do
        if inv memReqs.canPut
          then s.retry
          else do
            -- Address being accessed
            let memAddr = s.opA + s.immOrOpB
            -- Determine access width for memory request
            let accessWidth = getAccessWidth s.instr
            -- Is it a capability load/store?
            let isCapAccess = accessWidth .==. 3
            -- Number of bytes being accessed (for bounds check)
            let numBytes :: Bit 4 = 1 .<<. accessWidth
            -- Alignment check
            let alignmentMask :: Bit 3 = truncate (numBytes - 1)
            let alignmentOk =
                  (slice @2 @0 memAddr .&. alignmentMask) .==. 0
            -- Permission to load/store?
            let havePermission =
                  if s.opcode `is` [LOAD]
                    then permsA.permitLoad
                    else permsA.permitStore
            -- Convert capability to in-memory format for storing
            let (memCapTag, memCap) = toMem cB
            -- Possible exceptions
            let exceptionTable =
                  [ inv (isValidCap cA)
                      --> cheri_exc_tagViolation
                  , isSealed cA
                      --> cheri_exc_sealViolation
                  , s.opcode `is` [LOAD]
                     .&&. inv permsA.permitLoad
                      --> cheri_exc_permitLoadViolation
                  , s.opcode `is` [STORE]
                     .&&. inv permsA.permitStore
                      --> cheri_exc_permitStoreViolation
                  , s.opcode `is` [STORE]
                     .&&. isCapAccess
                     .&&. inv permsA.permitStoreCap
                     .&&. isValidCap cB
                      --> cheri_exc_permitStoreCapViolation
                  , s.opcode `is` [STORE]
                     .&&. isCapAccess
                     .&&. inv permsA.permitStoreLocalCap
                     .&&. isValidCap cB
                     .&&. permsB.global
                      --> cheri_exc_permitStoreLocalCapViolation
                  , inv alignmentOk
                      --> if s.opcode `is` [LOAD]
                            then exc_loadAddrMisaligned
                            else exc_storeAMOAddrMisaligned
                  , memAddr .<. baseA
                     .||. zeroExtend memAddr + zeroExtend numBytes .>. topA
                      --> cheri_exc_lengthViolation
                  ]
            -- Check for exception
            let isException = orList [cond | (cond, _) <- exceptionTable]
            -- Trigger exception
            if isException
              then trap s (priorityIf exceptionTable dontCare)
              else do
                -- Currently the memory subsystem doesn't issue store
                -- responses so we make sure to only suspend on a load
                let hasResp = s.opcode `is` [LOAD]
                         .||. s.opcode `is` [AMO] .&&. s.resultIndex .!=. 0
                when hasResp do s.suspend
                -- Send request to memory unit
                put memReqs
                  CapMemReq {
                    capMemReqIsCapAccess = isCapAccess
                  , capMemReqStd =
                      MemReq {
                        memReqAccessWidth =
                          -- Capability accesses are serialised
                          if isCapAccess then 2 else accessWidth
                      , memReqOp =
                          select
                            [ s.opcode `is` [LOAD]  --> memLoadOp
                            , s.opcode `is` [STORE] --> memStoreOp
                            , s.opcode `is` [AMO]   --> memAtomicOp
                            ]
                      , memReqAMOInfo =
                          AMOInfo {
                            amoOp = getAMO s.instr
                          , amoAcquire = getAcquire s.instr
                          , amoRelease = getRelease s.instr
                          , amoNeedsResp = hasResp
                          }
                      , memReqAddr = memAddr
                      , memReqData =
                          if isCapAccess then truncate memCap else s.opB
                      , memReqDataTagBit = isCapAccess .&&. memCapTag
                        -- Mask to be applied to tag bit of loaded capability
                      , memReqDataTagBitMask = permsA.permitLoadCap
                      , memReqIsUnsigned = getIsUnsignedLoad s.instr
                      , memReqIsFinal = true
                      }
                  , capMemReqUpperData = upper memCap
                  , capMemReqAbort = false
                  }

      -- Memory fence
      when (s.opcode `is` [FENCE]) do
        if memReqs.canPut
          then do
            s.suspend
            -- Send request to memory unit
            put memReqs
              CapMemReq {
                capMemReqStd =
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
              , capMemReqIsCapAccess = false
              , capMemReqUpperData = dontCare
              , capMemReqAbort = false
              }
         else s.retry

-- Execute stage (no shared bounds unit)
-- =====================================

-- Area-optimised implementation of CHERI instructions, for use when the
-- shared bounds unit is in operation
executeIxCHERIWithSharedBoundsUnit ::
     Maybe (Sink MulReq)
     -- ^ Optionally use multiplier to implement shifts
  -> Maybe CSRUnit
     -- ^ Access to CSRs
  -> Maybe (Sink CapMemReq)
     -- ^ Access to memory
  -> Sink SFUReq
     -- ^ Access to shared functional unit
  -> State
     -- ^ Pipeline state
  -> Action ()
executeIxCHERIWithSharedBoundsUnit m_shiftUnit m_csrUnit m_memReqs sfu s = do
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

  when (s.opcode `is` [ECALL]) do
    trap s exc_eCallFromM

  when (s.opcode `is` [EBREAK]) do
    trap s exc_breakpoint

  -- Control/status registers
  case m_csrUnit of
    Nothing -> return ()
    Just csrUnit ->
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

  -- Shorthands / shared logic for capability operands
  let cA = s.capA.capPipe
  let cB = s.capB.capPipe
  let topA = s.capA.capTop
  let baseA = s.capA.capBase
  let addrA = getAddr cA
  let permsA = getHardPerms cA
  let permsB = getHardPerms cB

  -- Capability inspection instructions
  -- ----------------------------------

  let isInspect = s.opcode `is`
        [ CGetPerm, CGetType, CGetTag
        , CGetSealed, CGetFlags, CGetAddr
        ]

  when isInspect do
    s.result <==
      select
        [ s.opcode `is` [CGetPerm] --> zeroExtend (getPerms cA)
        , s.opcode `is` [CGetType] -->
            let t = getType cA in
              if isSealedWithType cA then zeroExtend t else signExtend t
        , s.opcode `is` [CGetTag] --> zeroExtend (isValidCap cA)
        , s.opcode `is` [CGetSealed] --> zeroExtend (isSealed cA)
        , s.opcode `is` [CGetFlags] --> zeroExtend (getFlags cA)
        , s.opcode `is` [CGetAddr] --> addrA
        ]

  -- Special capability registers
  -- ----------------------------

  -- Non-compliant; always returns the almighty capability
  when (s.opcode `is` [CSpecialRW]) do
    s.resultCapMem <== almightyCapMemVal

  -- Other capability modification instructions
  -- ------------------------------------------

  -- Exception paths
  when (s.opcode `is` [CSetFlags, CSetAddr, CIncOffset]) do
    when (isValidCap cA .&&. isSealed cA) do
      trap s cheri_exc_sealViolation

  when (s.opcode `is` [CSealEntry, CAndPerm]) do
    if inv (isValidCap cA) then
      trap s cheri_exc_tagViolation
    else if isSealed cA then
      trap s cheri_exc_sealViolation
    else if s.opcode `is` [CSealEntry] .&&. inv permsA.permitExecute then
      trap s cheri_exc_permitExecuteViolation
    else
      return ()

  when (s.opcode `is` [CMove, CSealEntry, CSetFlags, CClearTag, CAndPerm]) do
    let newType  = if s.opcode `is` [CSealEntry]
                     then -2
                     else getType cA
    let newValid = if s.opcode `is` [CClearTag]
                     then 0
                     else isValidCap cA
    let newFlags = if s.opcode `is` [CSetFlags]
                     then lower s.opB
                     else getFlags cA
    let newPerms = if s.opcode `is` [CAndPerm]
                     then getPerms cA .&. lower s.opB
                     else getPerms cA
    s.resultCapMem <==
      ( flip setValidCapMem newValid
      $ flip setFlagsCapMem newFlags
      $ flip setTypeCapMem newType
      $ flip setPermsCapMem newPerms
      $ s.capA.capMem )

  -- Address-setting logic
  let useA = s.opcode `is` [CSetAddr, CIncOffset, JALR, LOAD, STORE, AMO]
  let oldCap = if useA then cA else s.pcc.capPipe
  let oldCapMem = if useA then s.capA.capMem else s.pcc.capMem
  let newAddr0 = s.immOrOpB +
        (if s.opcode `is` [CSetAddr] then 0 else getAddr oldCap)
  let newAddr1 = upper newAddr0 #
                   (if s.opcode `is` [JALR]
                      then 0 :: Bit 1 else lower newAddr0)
  let newCap = setAddr oldCap newAddr1
  let newCapMem = setValidCapMem
                    (setAddrUnsafeCapMem oldCapMem newAddr1)
                    (isValidCap newCap.value)

  when (s.opcode `is` [CSetAddr, CIncOffset, AUIPC]) do
    s.resultCapMem <== newCapMem

  -- Capability jumps/branches
  -- -------------------------

  when (s.opcode `is` [JAL, JALR]) do
    -- Exception path
    when (isSealed newCap.value .&&. inv (isSentry newCap.value)) do
      trap s cheri_exc_sealViolation

    -- Result path
    -- Use setAddrUnsafe: PC is in bounds therefore PC+4 is representable
    let linkCap = setAddrUnsafeCapMem s.pcc.capMem (s.pc.val + 4)
    s.resultCapMem <== setTypeCapMem linkCap (-2) {- Seal as sentry -}

  when ((s.opcode `is` [JAL, JALR]) .||. branch) do
    let newType = if s.opcode `is` [JALR] then -1 else getType newCap.value
    s.pccNewCapMem <== setTypeCapMem newCapMem newType {- Unseal -}

  -- Shared bounds unit instructions
  -- -------------------------------

  when (s.opcode `is` [CGetBase, CGetLen, CSetBounds, CSetBoundsImm,
                         CSetBoundsExact, CRRL, CRAM]) do
    if sfu.canPut
      then do
        s.suspend
        sfu.put
          SFUReq {
            kind = tag #bounds SFUBoundsReq {
                                 isGetBase = s.opcode `is` [CGetBase]
                               , isGetLen = s.opcode `is` [CGetLen]
                               , isSetBounds =
                                   s.opcode `is` [CSetBounds, CSetBoundsImm]
                               , isSetBoundsExact =
                                   s.opcode `is` [CSetBoundsExact]
                               , isCRAM = s.opcode `is` [CRAM]
                               , isCRRL = s.opcode `is` [CRRL]
                               }
          , opA = s.opA
          , opB = if s.opcode `is` [CSetBoundsImm]
                    then zeroExtend (getUImm s.instr)
                    else if s.opcode `is` [CSetBounds, CSetBoundsExact]
                           then s.opB else s.opA
          , capA = upper s.capA.capMem
          }
      else s.retry


  -- Memory access
  -- -------------

  case m_memReqs of
    Nothing -> return ()
    Just memReqs -> do
      when (s.opcode `is` [LOAD, STORE, AMO]) do
        if inv memReqs.canPut
          then s.retry
          else do
            -- Address being accessed
            let memAddr = newAddr0
            --let memAddr = s.opA + s.immOrOpB
            -- Determine access width for memory request
            let accessWidth = getAccessWidth s.instr
            -- Is it a capability load/store?
            let isCapAccess = accessWidth .==. 3
            -- Number of bytes being accessed (for bounds check)
            let numBytes :: Bit 4 = 1 .<<. accessWidth
            -- Alignment check
            let alignmentMask :: Bit 3 = truncate (numBytes - 1)
            let alignmentOk =
                  (slice @2 @0 memAddr .&. alignmentMask) .==. 0
            -- Permission to load/store?
            let havePermission =
                  if s.opcode `is` [LOAD]
                    then permsA.permitLoad
                    else permsA.permitStore
            -- Convert capability to in-memory format for storing
            let memCapTag :: Bit 1 = upper s.capB.capMem
            let memCap :: Bit 64 = truncate s.capB.capMem
            -- Possible exceptions
            let exceptionTable =
                  [ inv (isValidCap cA)
                      --> cheri_exc_tagViolation
                  , isSealed cA
                      --> cheri_exc_sealViolation
                  , s.opcode `is` [LOAD]
                     .&&. inv permsA.permitLoad
                      --> cheri_exc_permitLoadViolation
                  , s.opcode `is` [STORE]
                     .&&. inv permsA.permitStore
                      --> cheri_exc_permitStoreViolation
                  , s.opcode `is` [STORE]
                     .&&. isCapAccess
                     .&&. inv permsA.permitStoreCap
                     .&&. isValidCap cB
                      --> cheri_exc_permitStoreCapViolation
                  , s.opcode `is` [STORE]
                     .&&. isCapAccess
                     .&&. inv permsA.permitStoreLocalCap
                     .&&. isValidCap cB
                     .&&. permsB.global
                      --> cheri_exc_permitStoreLocalCapViolation
                  , inv alignmentOk
                      --> if s.opcode `is` [LOAD]
                            then exc_loadAddrMisaligned
                            else exc_storeAMOAddrMisaligned
{-
                  , memAddr .<. baseA
                      .||. zeroExtend memAddr + zeroExtend numBytes .>. topA
                      --> cheri_exc_lengthViolation
-}
                  , inv (isAccessInBounds newCap.value accessWidth)
                      --> cheri_exc_lengthViolation
                  ]
            -- Check for exception
            let isException = orList [cond | (cond, _) <- exceptionTable]
            when isException do
              trap s (priorityIf exceptionTable dontCare)
            -- Currently the memory subsystem doesn't issue store
            -- responses so we make sure to only suspend on a load
            let hasResp = s.opcode `is` [LOAD]
                     .||. s.opcode `is` [AMO] .&&. s.resultIndex .!=. 0
            when hasResp do s.suspend
            -- Send request to memory unit
            put memReqs
              CapMemReq {
                capMemReqIsCapAccess = isCapAccess
              , capMemReqStd =
                  MemReq {
                    memReqAccessWidth =
                      -- Capability accesses are serialised
                      if isCapAccess then 2 else accessWidth
                  , memReqOp =
                      select
                        [ s.opcode `is` [LOAD]  --> memLoadOp
                        , s.opcode `is` [STORE] --> memStoreOp
                        , s.opcode `is` [AMO]   --> memAtomicOp
                        ]
                  , memReqAMOInfo =
                      AMOInfo {
                        amoOp = getAMO s.instr
                      , amoAcquire = getAcquire s.instr
                      , amoRelease = getRelease s.instr
                      , amoNeedsResp = hasResp
                      }
                  , memReqAddr = memAddr
                  , memReqData = s.opB
                  , memReqDataTagBit = isCapAccess .&&. memCapTag
                    -- Mask to be applied to tag bit of loaded capability
                  , memReqDataTagBitMask = permsA.permitLoadCap
                  , memReqIsUnsigned = getIsUnsignedLoad s.instr
                  , memReqIsFinal = true
                  }
              , capMemReqUpperData = upper memCap
              , capMemReqAbort = isException
              }

      -- Memory fence
      when (s.opcode `is` [FENCE]) do
        if memReqs.canPut
          then do
            s.suspend
            -- Send request to memory unit
            put memReqs
              CapMemReq {
                capMemReqStd =
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
              , capMemReqIsCapAccess = false
              , capMemReqUpperData = dontCare
              , capMemReqAbort = false
              }
         else s.retry

-- Program counter capability
-- ==========================

-- | Check program counter capability
checkPCC :: Cap -> [(Bit 1, TrapCode)]
checkPCC cap =
  [ inv (isValidCap cap.capPipe)
      --> cheri_exc_tagViolation
  , isSealed cap.capPipe
      --> cheri_exc_sealViolation
  , inv perms.permitExecute
      --> cheri_exc_permitExecuteViolation
  , addr .<. cap.capBase .||. zeroExtend (addr + 4) .>. cap.capTop
      --> cheri_exc_lengthViolation
  ]
  where
    addr = getAddr cap.capPipe
    perms = getHardPerms cap.capPipe

-- CHERI instructions
-- ==================

-- Instructions that use the capability meta-data of their operand registers
instrsThatUseCapMetaDataA :: [Mnemonic]
instrsThatUseCapMetaDataA =
  [ AUIPC, JALR
  , LOAD, STORE, AMO
  , CGetPerm, CGetType
  , CGetBase, CGetLen
  , CGetTag, CGetSealed
  , CGetFlags, CGetAddr
  , CAndPerm, CSetFlags
  , CSetAddr , CIncOffset
  , CSetBounds, CSetBoundsImm, CSetBoundsExact
  , CMove, CClearTag
  , CSpecialRW, CSealEntry
  , CRRL, CRAM ]

usesCapMetaDataB :: MnemonicVec -> Bit 32 -> Bit 1
usesCapMetaDataB opcode instr =
  (opcode `is` [STORE]) .&&. getAccessWidth instr .==. 3
