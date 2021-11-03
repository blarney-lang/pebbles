module Pebbles.Instructions.RV32_M where

-- Blarney imports
import Blarney
import Blarney.Option
import Blarney.BitScan
import Blarney.SourceSink

-- Pebbles imports
import Pebbles.Pipeline.Interface
import Pebbles.Instructions.Mnemonics
import Pebbles.Instructions.Units.MulUnit
import Pebbles.Instructions.Units.DivUnit

-- Decode stage
-- ============

decodeM =
  [ "0000001 rs2<5> rs1<5> 0 mul<2> rd<5> 0110011" --> MUL
  , "0000001 rs2<5> rs1<5> 1 div<2> rd<5> 0110011" --> DIV
  ]

-- Selector functions
-- ==================

getMulInfo :: Bit 32 -> Bit 2
getMulInfo = makeFieldSelector decodeM "mul"

getDivInfo :: Bit 32 -> Bit 2
getDivInfo = makeFieldSelector decodeM "div"

-- Execute stage
-- =============

executeM :: MulUnit -> DivUnit -> State -> Action ()
executeM mulUnit divUnit s = do
  when (s.opcode `is` [MUL]) do
    if mulUnit.mulReqs.canPut
      then do
        info <- s.suspend
        let mulInfo = getMulInfo s.instr
        put (mulUnit.mulReqs)
          MulReq {
            mulReqInfo = info
          , mulReqA = s.opA
          , mulReqB = s.opB
          , mulReqLower = mulInfo .==. 0b00
          , mulReqUnsignedA = mulInfo .==. 0b11
          , mulReqUnsignedB = at @1 mulInfo
          }
      else s.retry

  when (s.opcode `is` [DIV]) do
    if divUnit.divReqs.canPut
      then do
        info <- s.suspend
        let divInfo = getDivInfo s.instr
        put (divUnit.divReqs)
          DivReq {
            divReqInfo = info
          , divReqNum = s.opA
          , divReqDenom = s.opB
          , divReqIsSigned = at @0 (inv divInfo)
          , divReqGetRemainder = at @1 divInfo
          }
      else s.retry
