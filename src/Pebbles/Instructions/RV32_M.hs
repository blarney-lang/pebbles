module Pebbles.Instructions.RV32_M where

-- Blarney imports
import Blarney
import Blarney.Option
import Blarney.BitScan
import Blarney.SourceSink

-- Pebbles imports
import Pebbles.Pipeline.Interface
import Pebbles.Instructions.MulUnit
import Pebbles.Instructions.DivUnit

-- Decode stage
-- ============

decodeM =
  [ "0000001 rs2<5> rs1<5> 0 mul<2> rd<5> 0110011" --> "MUL"
  , "0000001 rs2<5> rs1<5> 1 div<2> rd<5> 0110011" --> "DIV"
  ]

-- Execute stage
-- =============

executeM :: MulUnit -> DivUnit -> State -> Action ()
executeM mulUnit divUnit s = do
  when (s.opcode `is` ["MUL"]) do
    if mulUnit.mulReqs.canPut
      then do
        id <- s.suspend
        let mulInfo :: Option (Bit 2) = getField (s.fields) "mul"
        put (mulUnit.mulReqs)
          MulReq {
            mulReqId = id
          , mulReqA = s.opA
          , mulReqB = s.opB
          , mulReqLower = mulInfo.val .==. 0b00
          , mulReqUnsignedA = mulInfo.val .==. 0b11
          , mulReqUnsignedB = at @1 (mulInfo.val)
          }
      else s.retry

  when (s.opcode `is` ["DIV"]) do
    if mulUnit.mulReqs.canPut
      then do
        id <- s.suspend
        let divInfo :: Option (Bit 2) = getField (s.fields) "div"
        put (divUnit.divReqs)
          DivReq {
            divReqId = id
          , divReqNum = s.opA
          , divReqDenom = s.opB
          , divReqIsSigned = at @0 (divInfo.val.inv)
          , divReqGetRemainder = at @1 (divInfo.val)
          }
      else s.retry
