module Pebbles.Instructions.RV32_xCHERI where

-- Blarney imports
import Blarney
import Blarney.Stmt
import Blarney.Queue
import Blarney.Stream
import Blarney.SourceSink

-- Pebbles imports
import Pebbles.CSRs.CSRUnit
import Pebbles.CSRs.TrapCodes
import Pebbles.Memory.Interface
import Pebbles.Pipeline.Interface
import Pebbles.Instructions.Mnemonics

-- CHERI imports
import CHERI.CapLib

-- Haskell imports
import Numeric (showHex)

-- Decode stage
-- ============

decodeCHERI =
  [ "1111111 00000 rs1<5> 000 rd<5> 1011011" --> CGetPerm
  , "0001101 rs2<5> rs1<5> 000 rd<5> 1011011" --> CAndPerm
  ]

-- Execute stage
-- =============

executeCHERI ::
     CSRUnit
     -- ^ Access to CSRs
  -> MemUnit InstrInfo
     -- ^ Access to memory
  -> State
     -- ^ Pipeline state
  -> Action ()
executeCHERI csrUnit memUnit s = do

  when (s.opcode `is` [CGetPerm]) do
    s.resultCap <== zeroExtend (s.capA.getPerms)

  when (s.opcode `is` [CAndPerm]) do
    if s.capA.isValidCap.inv then
      trap s cheri_exc_tagViolation
    else if s.capA.isSealed then
      trap s cheri_exc_sealViolation
    else do
      let newPerms = s.capA.getPerms .&. s.opB.lower
      s.resultCap <== setPerms (s.capA) newPerms

-- Program counter capability
-- ==========================

-- | Check program counter capability
checkPCC :: InternalCap -> Bit 1
checkPCC cap =
       isValidCap cap
  .&&. inv (isSealed cap)
  .&&. perms.permitExecute
  .&&. perms.permitLoad
  .&&. addr .>=. getBase cap
  .&&. zeroExtend (addr + 4) .<=. getTop cap
  where
    addr = getAddr cap
    perms = getHardPerms cap
