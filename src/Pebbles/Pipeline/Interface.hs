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
    -- | Instruction id
    instrId :: InstrId
    -- | Destination register of suspended instruction
  , instrDest :: RegId
  } deriving (Generic, Interface, Bits)

-- | Resume request to pipeline for multi-cycle instructions
data ResumeReq =
  ResumeReq {
    -- | Instruction info from the original suspend call
    resumeReqInfo :: InstrInfo
    -- | Data representing the result of the suspended operation
  , resumeReqData :: Bit 32
  } deriving (Generic, Bits)

-- | Pipeline state, visisble to the execute stage
data State =
  State {
    -- | Current instruction.
    instr :: Bit 32

    -- | Source operands. These contain the values of the two source registers.
  , opA :: Bit 32
  , opB :: Bit 32
  , opBorImm :: Bit 32

    -- | Program counter interface. This may be read, to obtain the PC
    -- of the currently executing instruction.  It may be written,
    -- to modify the PC. If unwritten, the pipeline implicity
    -- updates the PC to point to the next instruction in memory.
  , pc :: ReadWrite (Bit 32)

    -- | Instruction result interface.  Writing to this modifies 
    -- the destination register.
  , result :: WriteOnly (Bit 32)

    -- | Call this to implement a multi-cycle instruction.
    -- Results are returned via resume stage.
  , suspend :: Action InstrInfo
    -- | Call this if the instruction cannot currently be executed
    -- (perhaps resources are not currently available).
  , retry :: Action ()
    -- Mnemonic(s) for current instruction identified by the decoder
  , opcode :: MnemonicVec
  } deriving (Generic, Interface)

-- | Upper bound on number of instruction mnemonics used by the decoder
type MaxMnemonics = 64

-- | Bit vector indentifying one or more active mnemonics
type MnemonicVec = Bit MaxMnemonics
