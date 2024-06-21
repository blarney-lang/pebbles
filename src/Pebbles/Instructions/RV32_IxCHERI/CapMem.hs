module Pebbles.Instructions.RV32_IxCHERI.CapMem where

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
import Pebbles.Instructions.RV32_IxCHERI

-- CHERI imports
import CHERI.CapLib

-- Area-optimised implementation of CHERI instructions, for use when the
-- shared bounds unit is in operation
executeIxCHERI_CapMem ::
     Maybe (Sink MulReq)
     -- ^ Optionally use multiplier to implement shifts
  -> Maybe CSRUnit
     -- ^ Access to CSRs
  -> Maybe (Sink CapMemReq)
     -- ^ Access to memory
  -> State
     -- ^ Pipeline state
  -> Action ()
executeIxCHERI_CapMem m_shiftUnit m_csrUnit m_memReqs s = do
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
  let oldCap =
        if s.opcode `is` [CSetAddr, CIncOffset, JALR, LOAD, STORE, AMO]
          then cA else s.pcc.capPipe
  let oldCapMem =
        if s.opcode `is` [CSetAddr, CIncOffset, JALR, LOAD, STORE, AMO]
          then s.capA.capMem else s.pcc.capMem
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
                  , inv (isValidCap newCap.value .&&.
                           isInBounds newCap.value false)
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
                      , memReqData = s.opB
                          --if isCapAccess then truncate memCap else s.opB
                      , memReqDataTagBit = isCapAccess .&&. memCapTag
                        -- Mask to be applied to tag bit of loaded capability
                      , memReqDataTagBitMask = permsA.permitLoadCap
                      , memReqIsUnsigned = getIsUnsignedLoad s.instr
                      , memReqIsFinal = true
                      }
                  , capMemReqUpperData = upper memCap
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
              }
         else s.retry
