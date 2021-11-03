-- RISC-V atomics extension
module Pebbles.Instructions.RV32_A
  ( decodeA
  , executeA
  ) where

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

executeA :: Sink (MemReq InstrInfo) -> State -> Action ()
executeA memReqs s = do
  -- Atomic memory access
  when (s.opcode `is` [AMO]) do
    if memReqs.canPut
      then do
        -- Is response needed?
        let needsResp = getDestReg s.instr .!=. 0
        -- Suspend if response needed
        info <- whenR needsResp (s.suspend)
        -- Send request to memory unit
        put memReqs
          MemReq {
            memReqId = info
          , memReqAccessWidth = 0b10
          , memReqOp = memAtomicOp
          , memReqAMOInfo =
              AMOInfo {
                amoOp = getAMO s.instr
              , amoAcquire = getAcquire s.instr
              , amoRelease = getRelease s.instr
              , amoNeedsResp = needsResp
              }
          , memReqAddr = s.opA
          , memReqData = s.opB
          , memReqDataTagBit = 0
          , memReqIsUnsigned = true
          , memReqIsFinal = true
          }
      else s.retry
