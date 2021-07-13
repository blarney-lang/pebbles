module Pebbles.Instructions.RV32_xCHERI where

-- Blarney imports
import Blarney

-- CHERI imports
import CHERI.CapLib

-- | Check program counter capability
checkPCC :: InternalCap -> Bit 1
checkPCC cap =
       isValidCap cap
  .&&. isUnsealed cap
  .&&. perms.permitExecute
  .&&. perms.permitLoad
  .&&. addr .>=. getBase cap
  .&&. zeroExtend (addr + 4) .<=. getTop cap
  where
    addr = getAddr cap
    perms = getHardPerms cap
