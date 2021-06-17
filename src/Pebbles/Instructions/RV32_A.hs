-- RISC-V atomics extension
module Pebbles.Instructions.RV32_A where

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.BitScan
import Blarney.SourceSink

-- Pebbles imports
import Pebbles.CSRs.CSRUnit
import Pebbles.Memory.Interface
import Pebbles.Pipeline.Interface
import Pebbles.Instructions.Mnemonics

-- Decode stage
-- ============

decodeA =
  [ "amo<5> aq<1> rl<1> rs2<5> rs1<5> 010 rd<5> 0101111" --> AMO
  ]

-- Field selectors
-- ===============

getAMO :: Bit 32 -> Bit 5
getAMO = makeFieldSelector decodeA "amo"

getAcquire :: Bit 32 -> Bit 1
getAcquire = makeFieldSelector decodeA "aq"

getRelease :: Bit 32 -> Bit 1
getRelease = makeFieldSelector decodeA "rl"

getDestReg :: Bit 32 -> Bit 5
getDestReg = makeFieldSelector decodeA "rd"

-- Execute stage
-- =============

executeA :: MemUnit InstrInfo -> State -> Action ()
executeA memUnit s = do
  -- Atomic memory access
  when (s.opcode `is` [AMO]) do
    if memUnit.memReqs.canPut
      then do
        -- Is response needed?
        let needsResp = s.instr.getDestReg .!=. 0
        -- Suspend if response needed
        info <- whenR needsResp (s.suspend)
        -- Send request to memory unit
        put (memUnit.memReqs)
          MemReq {
            memReqId = info
          , memReqAccessWidth = 0b10
          , memReqOp = memAtomicOp
          , memReqAMOInfo =
              AMOInfo {
                amoOp = s.instr.getAMO
              , amoAcquire = s.instr.getAcquire
              , amoRelease = s.instr.getRelease
              , amoNeedsResp = needsResp
              }
          , memReqAddr = s.opA
          , memReqData = s.opB
          , memReqDataTagBit = 0
          , memReqIsUnsigned = true
          , memReqIsFinal = true
          }
      else s.retry
