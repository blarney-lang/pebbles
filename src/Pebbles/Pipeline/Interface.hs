module Pebbles.Pipeline.Interface where

-- 32-bit processor pipeline interface

-- Blarney imports
import Blarney
import Blarney.Stream
import Blarney.BitScan

-- General imports
import qualified Data.Map as Map

-- | Instructions
type Instr = Bit 32

-- | Register identifiers
type RegId = Bit 5

-- | Instruction id, for instruction suspension/resumption
-- Currently limited to a max of 64 outstanding suspensions
type InstrIdWidth = 6
type InstrId = Bit InstrIdWidth

-- | Instruction info for suspension/resumption
data InstrInfo =
  InstrInfo {
    instrId :: InstrId
    -- ^ Instruction id
  , instrDest :: RegId
    -- ^ Destination register of suspended instruction
  } deriving (Generic, Interface, Bits)

-- | Resume request to pipeline for multi-cycle instructions
data ResumeReq =
  ResumeReq {
    resumeReqInfo :: InstrInfo
    -- ^ Instruction info from the original suspend call
  , resumeReqData :: Bit 32
    -- ^ Data representing the result of the suspended operation
  } deriving (Generic, Interface, Bits)

-- | Pipeline state, visisble to the execute stage
data State =
  State {
    instr :: Bit 32
    -- ^ Current instruction

  , opA :: Bit 32
  , opB :: Bit 32
  , opBorImm :: Bit 32
    -- ^ Source operands. These contain the values of the two source registers.

  , opAIndex :: RegId
  , opBIndex :: RegId
  , resultIndex :: RegId
    -- ^ Register file indices.

  , pc :: ReadWrite (Bit 32)
    -- ^ Program counter interface. This may be read, to obtain the PC
    -- of the currently executing instruction.  It may be written,
    -- to modify the PC. If unwritten, the pipeline implicity
    -- updates the PC to point to the next instruction in memory.

  , result :: WriteOnly (Bit 32)
    -- ^ Instruction result interface.  Writing to this modifies
    -- the destination register.

  , suspend :: Action InstrInfo
    -- ^ Call this to implement a multi-cycle instruction.
    -- Results are returned via resume stage.

  , retry :: Action ()
    -- ^ Call this if the instruction cannot currently be executed
    -- (perhaps resources are not currently available).

  , opcode :: MnemonicVec
    -- ^ Mnemonic(s) for current instruction identified by the decoder
  } deriving (Generic, Interface)

-- | Upper bound on number of instruction mnemonics used by the decoder
type MaxMnemonics = 64

-- | Bit vector indentifying one or more active mnemonics
type MnemonicVec = Bit MaxMnemonics

-- | Interface to pipeline's execute stage
data ExecuteStage =
  ExecuteStage {
    execute :: Action ()
    -- ^ Trigger execute stage
  , resumeReqs :: Stream ResumeReq
    -- ^ Resume requests for instructions that suspend
  } deriving (Generic, Interface)
