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
  , "0010100 rs2<5> rs1<5> 000 rd<5> 1011011" --> CSub
  , "1111111 01010  rs1<5> 000 rd<5> 1011011" --> CMove
  , "1111111 01011  rs1<5> 000 rd<5> 1011011" --> CClearTag
  , "0000001 scr<5> rs1<5> 000 rd<5> 1011011" --> CSpecialRW
  , "1111111 10001  rs1<5> 000 rd<5> 1011011" --> CSealEntry
  , "imm[31:12] rd<5> 0010111"                --> AUIPCC
  , "1111111 01100  rs1<5> 000 rd<5> 1011011" --> CJALR
  , "imm[11:0] rs1<5> ul<1> aw<2> rd<5> 0000011" --> LOAD
  , "imm[11:5] rs2<5> rs1<5> 0 aw<2> imm[4:0] 0100011" --> STORE
  ]

decodeCHERI_A =
  [ "imm<0> amo<5> aq<1> rl<1> rs2<5> rs1<5> 010 rd<5> 0101111" --> AMO
  ]

-- Field selectors
-- ===============

getAccessWidth :: Bit 32 -> Bit 2
getAccessWidth = makeFieldSelector decodeCHERI "aw"

getIsUnsignedLoad :: Bit 32 -> Bit 1
getIsUnsignedLoad = makeFieldSelector decodeCHERI "ul"

getAMO :: Bit 32 -> Bit 5
getAMO = makeFieldSelector decodeCHERI_A "amo"

getAcquire :: Bit 32 -> Bit 1
getAcquire = makeFieldSelector decodeCHERI_A "aq"

getRelease :: Bit 32 -> Bit 1
getRelease = makeFieldSelector decodeCHERI_A "rl"

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

  -- Shorthands / shared logic for capability operands
  let cA = s.capA.capInternal
  let cB = s.capB.capInternal
  let topA = s.capA.capTop
  let lenA = s.capA.capLength
  let baseA = s.capA.capBase
  let addrA = cA.getAddr
  let offsetA = s.capA.capOffset
  let permsA = cA.getHardPerms
  let permsB = cB.getHardPerms

  -- Capability inspection instructions
  -- ----------------------------------

  let isInspect = s.opcode `is`
        [ CGetPerm, CGetType, CGetBase, CGetLen, CGetTag
        , CGetSealed, CGetOffset, CGetFlags, CGetAddr
        ]

  when isInspect do
    s.result <==
      select
        [ s.opcode `is` [CGetPerm] --> cA.getPerms.zeroExtend
        , s.opcode `is` [CGetType] -->
            let t = cA.getType in
              if cA.isSealedWithType then zeroExtend t else signExtend t
        , s.opcode `is` [CGetBase] --> baseA
        , s.opcode `is` [CGetLen] -->
              if at @32 lenA then ones else lower lenA
        , s.opcode `is` [CGetTag] --> cA.isValidCap.zeroExtend
        , s.opcode `is` [CGetSealed] --> cA.isSealed.zeroExtend
        , s.opcode `is` [CGetOffset] --> offsetA
        , s.opcode `is` [CGetFlags] --> cA.getFlags.zeroExtend
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
    let newCap = setBounds cA (s.opBorImm)
    let needExact = s.opcode `is` [CSetBoundsExact]

    -- Exception path
    if cA.isValidCap.inv then
      trap s cheri_exc_tagViolation
    else if cA.isSealed then
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
    when (cA.isValidCap .&&. cA.isSealed) do
      trap s cheri_exc_sealViolation

  when (s.opcode `is` [CSealEntry, CAndPerm]) do
    if cA.isValidCap.inv then
      trap s cheri_exc_tagViolation
    else if cA.isSealed then
      trap s cheri_exc_sealViolation
    else if s.opcode `is` [CSealEntry] .&&. permsA.permitExecute.inv then
      trap s cheri_exc_permitExecuteViolation
    else
      return ()

  -- Result paths
  when (s.opcode `is` [CSetAddr, CSetOffset, CIncOffset, AUIPCC]) do
    let oldCap = if s.opcode `is` [AUIPCC] then s.pcc.val else cA
    let newAddr =
          select
            [ s.opcode `is` [CSetAddr] --> s.opB
            , s.opcode `is` [CIncOffset, AUIPCC] --> getAddr oldCap + s.opBorImm
            , s.opcode `is` [CSetOffset] --> baseA + s.opBorImm
            ]
    let newCap = setAddr oldCap newAddr
    s.resultCap <== newCap.value

  when (s.opcode `is` [CMove, CSealEntry, CSetFlags, CClearTag, CAndPerm]) do
    let newType  = if s.opcode `is` [CSealEntry]
                     then -2
                     else cA.getType
    let newValid = if s.opcode `is` [CClearTag]
                     then 0
                     else cA.isValidCap
    let newFlags = if s.opcode `is` [CSetFlags]
                     then s.opB.lower
                     else cA.getFlags
    let newPerms = if s.opcode `is` [CAndPerm]
                     then cA.getPerms .&. s.opB.lower
                     else cA.getPerms
    s.resultCap <==
      ( flip setValidCap newValid
      $ flip setFlags newFlags
      $ flip setType newType
      $ flip setPerms newPerms
      $ cA )

  -- Capability subtraction
  -- ----------------------

  when (s.opcode `is` [CSub]) do
    s.result <== addrA - cB.getAddr

  -- Capability jump
  -- ---------------

  when (s.opcode `is` [CJALR]) do
    -- Exception path
    when (cA.isSealed .&&. cA.isSentry.inv) do
      trap s cheri_exc_sealViolation

    -- Result path
    let linkCap = setAddr (s.pcc.val) (s.pc.val + 4)
    s.resultCap <== setType (linkCap.value) (-2) {- Seal as sentry -}
    s.pc <== cA.getAddr.upper # (0 :: Bit 1)
    s.pcc <== setType cA (-1) {- Unseal -}

  -- Memory access
  -- -------------

  when (s.opcode `is` [LOAD, STORE, AMO]) do
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
              if s.opcode `is` [LOAD]
                then permsA.permitLoad
                else permsA.permitStore
        -- Convert capability to in-memory format for storing
        let (memCapTag, memCap) = cB.toMem
        -- Possible exceptions
        let exceptionTable =
              [ cA.isValidCap.inv
                  --> cheri_exc_tagViolation
              , cA.isSealed
                  --> cheri_exc_sealViolation
              , s.opcode `is` [LOAD]
                 .&&. permsA.permitLoad.inv
                  --> cheri_exc_permitLoadViolation
              , s.opcode `is` [STORE]
                 .&&. permsA.permitStore.inv
                  --> cheri_exc_permitStoreViolation
              , s.opcode `is` [STORE]
                 .&&. isCapAccess
                 .&&. permsA.permitStoreCap.inv
                 .&&. cB.isValidCap
                  --> cheri_exc_permitStoreCapViolation
              , s.opcode `is` [STORE]
                 .&&. isCapAccess
                 .&&. permsA.permitStoreLocalCap.inv
                 .&&. cB.isValidCap
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
            -- Currently the memory subsystem doesn't issue store responses
            -- so we make sure to only suspend on a load
            let hasResp = s.opcode `is` [LOAD]
                     .||. s.opcode `is` [AMO] .&&. s.resultIndex .!=. 0
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
                      select
                        [ s.opcode `is` [LOAD]  --> memLoadOp
                        , s.opcode `is` [STORE] --> memStoreOp
                        , s.opcode `is` [AMO]   --> memAtomicOp
                        ]
                  , memReqAMOInfo =
                      AMOInfo {
                        amoOp = s.instr.getAMO
                      , amoAcquire = s.instr.getAcquire
                      , amoRelease = s.instr.getRelease
                      , amoNeedsResp = hasResp
                      }
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
