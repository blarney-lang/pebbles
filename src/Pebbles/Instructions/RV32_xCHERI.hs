-- Subset of CHERI (capability mode only)

module Pebbles.Instructions.RV32_xCHERI where

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

-- CHERI imports
import CHERI.CapLib

-- Decode stage
-- ============

decodeCHERI =
  [ "1111111 00000  rs1<5> 000 rd<5> 1011011" --> CGetPerm
  , "1111111 00001  rs1<5> 000 rd<5> 1011011" --> CGetType
  , "1111111 00010  rs1<5> 000 rd<5> 1011011" --> CGetBase
  , "1111111 00011  rs1<5> 000 rd<5> 1011011" --> CGetLen
  , "1111111 00100  rs1<5> 000 rd<5> 1011011" --> CGetTag
  , "1111111 00101  rs1<5> 000 rd<5> 1011011" --> CGetSealed
  , "1111111 00110  rs1<5> 000 rd<5> 1011011" --> CGetOffset
  , "1111111 00111  rs1<5> 000 rd<5> 1011011" --> CGetFlags
  , "1111111 01111  rs1<5> 000 rd<5> 1011011" --> CGetAddr
  , "0001101 rs2<5> rs1<5> 000 rd<5> 1011011" --> CAndPerm
  , "0001110 rs2<5> rs1<5> 000 rd<5> 1011011" --> CSetFlags
  , "0001111 rs2<5> rs1<5> 000 rd<5> 1011011" --> CSetOffset
  , "0010000 rs2<5> rs1<5> 000 rd<5> 1011011" --> CSetAddr
  , "0010001 rs2<5> rs1<5> 000 rd<5> 1011011" --> CIncOffset
  , "imm[11:0]      rs1<5> 001 rd<5> 1011011" --> CIncOffset
  , "0001000 rs2<5> rs1<5> 000 rd<5> 1011011" --> CSetBounds
  , "imm[11:0]      rs1<5> 010 rd<5> 1011011" --> CSetBounds
  , "0001001 rs2<5> rs1<5> 000 rd<5> 1011011" --> CSetBoundsExact
  , "imm[31:12] rd<5> 0010111" --> AUIPCC
  , "1111111 01100  rs1<5> 000 rd<5> 1011011" --> CJALR
  , "0010100 rs2<5> rs1<5> 000 rd<5> 1011011" --> CSub
  , "1111111 01010  rs1<5> 000 rd<5> 1011011" --> CMove
  , "1111111 01011  rs1<5> 000 rd<5> 1011011" --> CClearTag
  , "0000001 scr<5> rs1<5> 000 rd<5> 1011011" --> CSpecialRW
  , "imm[11:0] rs1<5> ul<1> aw<2> rd<5> 0000011" --> CLoad
  , "imm[11:5] rs2<5> rs1<5> 0 aw<2> imm[4:0] 0100011" --> CStore
  , "1111111 10001 rs1<5> 000 rd<5> 1011011" --> CSealEntry
  ]

-- Field selectors
-- ===============

getAccessWidth :: Bit 32 -> Bit 2
getAccessWidth = makeFieldSelector decodeCHERI "aw"

getIsUnsignedLoad :: Bit 32 -> Bit 1
getIsUnsignedLoad = makeFieldSelector decodeCHERI "ul"

-- Execute stage
-- =============

executeCHERI ::
     CSRUnit
     -- ^ Access to CSRs
  -> Sink (CapMemReq InstrInfo)
     -- ^ Access to memory
  -> State
     -- ^ Pipeline state
  -> Action ()
executeCHERI csrUnit memReqs s = do

  -- Shared logic for capabilities
  let topA = s.capA.getTop
  let baseA = s.capA.getBase
  let addrA = s.capA.getAddr
  let permsA = s.capA.getHardPerms
  let permsB = s.capB.getHardPerms

  -- Capability inspection instructions
  -- ----------------------------------

  let isInspect = s.opcode `is`
        [ CGetPerm, CGetType, CGetBase, CGetLen, CGetTag
        , CGetSealed, CGetOffset, CGetFlags, CGetAddr
        ]

  when isInspect do
    s.result <==
      select
        [ s.opcode `is` [CGetPerm] --> s.capA.getPerms.zeroExtend
        , s.opcode `is` [CGetType] -->
            let t = s.capA.getType in
              if s.capA.isSealedWithType then zeroExtend t else signExtend t
        , s.opcode `is` [CGetBase] --> baseA
        , s.opcode `is` [CGetLen] -->
            let len = s.capA.getLength in
              if at @32 len then ones else lower len
        , s.opcode `is` [CGetTag] --> s.capA.isValidCap.zeroExtend
        , s.opcode `is` [CGetSealed] --> s.capA.isSealed.zeroExtend
        , s.opcode `is` [CGetOffset] --> s.capA.getOffset
        , s.opcode `is` [CGetFlags] --> s.capA.getFlags.zeroExtend
        , s.opcode `is` [CGetAddr] --> addrA
        ]

  -- Special capability registers
  -- ----------------------------

  -- Non-compliant; always returns the almighty capability
  when (s.opcode `is` [CSpecialRW]) do
    s.resultCap <== almightyCapVal

  -- Bounds setting instructions
  -- ---------------------------

  when (s.opcode `is` [CSetBounds, CSetBoundsExact]) do
    let newCap = setBounds (s.capA) (s.opBorImm)
    let needExact = s.opcode `is` [CSetBoundsExact]

    -- Exception path
    if s.capA.isValidCap.inv then
      trap s cheri_exc_tagViolation
    else if s.capA.isSealed then
      trap s cheri_exc_sealViolation
    else if addrA .<. baseA .||.
              zeroExtend addrA + zeroExtend (s.opBorImm) .>. topA then
      trap s cheri_exc_lengthViolation
    else if needExact .&&. newCap.exact.inv then
      trap s cheri_exc_representabilityViolation
    else return ()

    -- Result path
    s.resultCap <== newCap.value

  -- Other capability modification instructions
  -- ------------------------------------------

  -- Exception paths
  when (s.opcode `is` [CSetFlags, CSetOffset, CSetAddr, CIncOffset]) do
    when (s.capA.isValidCap .&&. s.capA.isSealed) do
      trap s cheri_exc_sealViolation

  when (s.opcode `is` [CSealEntry, CAndPerm]) do
    if s.capA.isValidCap.inv then
      trap s cheri_exc_tagViolation
    else if s.capA.isSealed then
      trap s cheri_exc_sealViolation
    else if s.opcode `is` [CSealEntry] .&&. permsA.permitExecute.inv then
      trap s cheri_exc_permitExecuteViolation
    else
      return ()

  -- Result paths
  when (s.opcode `is` [CSetOffset, CIncOffset]) do
    let newCap = modifyOffset (s.capA) (s.opBorImm)
                   (s.opcode `is` [CIncOffset])
    s.resultCap <== newCap.value

  when (s.opcode `is` [CSetAddr]) do
    let newCap = setAddr (s.capA) (s.opB)
    s.resultCap <== newCap.value

  when (s.opcode `is` [CMove, CSealEntry, CSetFlags, CClearTag, CAndPerm]) do
    let newType  = if s.opcode `is` [CSealEntry]
                     then -2
                     else s.capA.getType
    let newValid = if s.opcode `is` [CClearTag]
                     then 0
                     else s.capA.isValidCap
    let newFlags = if s.opcode `is` [CSetFlags]
                     then s.opB.lower
                     else s.capA.getFlags
    let newPerms = if s.opcode `is` [CAndPerm]
                     then s.capA.getPerms .&. s.opB.lower
                     else s.capA.getPerms
    s.resultCap <==
      ( flip setValidCap newValid
      $ flip setFlags newFlags
      $ flip setType newType
      $ flip setPerms newPerms
      $ s.capA )

  -- Capability subtraction
  -- ----------------------

  when (s.opcode `is` [CSub]) do
    s.result <== addrA - s.capB.getAddr

  -- PCC-related instructions
  -- ------------------------

  when (s.opcode `is` [AUIPCC]) do
    let newCap = incOffset (s.pcc.val) (s.opBorImm)
    s.resultCap <== newCap.value

  when (s.opcode `is` [CJALR]) do
    -- Exception path
    when (s.capA.isSealed .&&. s.capA.isSentry.inv) do
      trap s cheri_exc_sealViolation

    -- Result path
    let linkCap = setAddr (s.pcc.val) (s.pc.val + 4)
    s.resultCap <== setType (linkCap.value) (-2) {- Seal as sentry -}
    s.pc <== s.capA.getAddr.upper # (0 :: Bit 1)
    s.pcc <== setType (s.capA) (-1) {- Unseal -}

  -- Memory access
  -- -------------

  when (s.opcode `is` [CLoad, CStore]) do
    if memReqs.canPut.inv
      then s.retry
      else do
        -- Address being accessed
        let memAddr = addrA + s.opBorImm
        -- Determine access width for memory request
        let accessWidth = s.instr.getAccessWidth
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
              if s.opcode `is` [CLoad]
                then permsA.permitLoad
                else permsA.permitStore
        -- Convert capability to in-memory format for storing
        let (memCapTag, memCap) = s.capB.toMem
        -- Possible exceptions
        let exceptionTable =
              [ s.capA.isValidCap.inv
                  --> cheri_exc_tagViolation
              , s.capA.isSealed
                  --> cheri_exc_sealViolation
              , s.opcode `is` [CLoad]
                 .&&. permsA.permitLoad.inv
                  --> cheri_exc_permitLoadViolation
              , s.opcode `is` [CStore]
                 .&&. permsA.permitStore.inv
                  --> cheri_exc_permitStoreViolation
              , s.opcode `is` [CStore]
                 .&&. isCapAccess
                 .&&. permsA.permitStoreCap.inv
                 .&&. s.capB.isValidCap
                  --> cheri_exc_permitStoreCapViolation
              , s.opcode `is` [CStore]
                 .&&. isCapAccess
                 .&&. permsA.permitStoreLocalCap.inv
                 .&&. s.capB.isValidCap
                 .&&. permsB.global
                  --> cheri_exc_permitStoreLocalCapViolation
              , inv alignmentOk
                  --> if s.opcode `is` [CLoad]
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
            -- Currently the memory subsystem doesn't issue store responses
            -- so we make sure to only suspend on a load
            let hasResp = s.opcode `is` [CLoad]
            info <- whenR hasResp (s.suspend)
            -- Send request to memory unit
            put memReqs
              CapMemReq {
                capMemReqIsCapAccess = isCapAccess
              , capMemReqStd =
                  MemReq {
                    memReqId =
                      -- Mask to be applied to tag bit of loaded capability
                      info { instrTagMask = permsA.permitLoadCap }
                  , memReqAccessWidth =
                      -- Capability accesses are serialised
                      if isCapAccess then 2 else accessWidth
                  , memReqOp =
                      if s.opcode `is` [CLoad] then memLoadOp else memStoreOp
                  , memReqAMOInfo = dontCare
                  , memReqAddr = memAddr
                  , memReqData =
                      if isCapAccess then lower memCap else s.opB
                  , memReqDataTagBit = isCapAccess .&&. memCapTag
                  , memReqIsUnsigned = s.instr.getIsUnsignedLoad
                  , memReqIsFinal = true
                  }
              , capMemReqUpperData = upper memCap
              }

-- Program counter capability
-- ==========================

-- | Check program counter capability
checkPCC :: InternalCap -> [(Bit 1, TrapCode)]
checkPCC cap =
  [ inv (isValidCap cap)
      --> cheri_exc_tagViolation
  , isSealed cap
      --> cheri_exc_sealViolation
  , perms.permitExecute.inv
      --> cheri_exc_permitExecuteViolation
  , addr .<. getBase cap .||. zeroExtend (addr + 4) .>. getTop cap
      --> cheri_exc_lengthViolation
  ]
  where
    addr = getAddr cap
    perms = getHardPerms cap
