module Pebbles.Pipeline.Interface where

-- 32-bit processor pipeline interface

-- Blarney imports
import Blarney
import Blarney.Stream
import Blarney.BitScan

-- General imports
import qualified Data.Map as Map

-- Instructions
type Instr = Bit 32

-- Register identifiers
type RegId = Bit 5

-- Pipeline configuration
data Config =
  Config {
    -- Decode table
    decodeStage :: [(String, String)]
    -- Action for execute stage
  , executeStage :: State -> Action ()
    -- Resumption for multi-cycle instructions
  , resumeStage :: Stream ResumeReq
  }

-- Instruction identifier for suspension/resumption
-- Currently limited to a max of 64 outstanding suspensions
type InstrId = Bit 6

-- Resume request to pipeline for multi-cycle instructions
data ResumeReq =
  ResumeReq {
    -- Unique identifier that was given by the suspend call
    resumeReqId :: InstrId
    -- Data representing the result of the suspended operation
  , resumeReqData :: Bit 32
  } deriving (Generic, Bits)

-- Pipeline state, visisble to the execute stage
data State =
  State {
    -- Current instruction
    instr :: Bit 32
    -- Source operands
  , opA :: Bit 32
  , opB :: Bit 32
  , opBorImm :: Bit 32
    -- Program counter interface
  , pc :: ReadWrite (Bit 32)
    -- Write the instruction result
  , result :: WriteOnly (Bit 32)
    -- Call this to implement a multi-cycle instruction
    -- Results are returned via resume stage
  , suspend :: Action InstrId
    -- Call this if instruction cannot currently be executed
    -- (Perhaps resources are not currently available)
  , retry :: Action ()
    -- Result of instruction decode
  , opcode :: TagMap String
  , fields :: FieldMap
  }
