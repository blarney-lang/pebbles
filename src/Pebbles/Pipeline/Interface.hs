module Pebbles.Pipeline.Interface where

-- 32-bit processor pipeline interface

-- Blarney imports
import Blarney
import Blarney.Stream
import Blarney.Option
import Blarney.BitScan

-- Pebbles imports
import Pebbles.CSRs.TrapCodes

-- CHERI imports
import CHERI.CapLib

-- General imports
import qualified Data.Map as Map

-- | Instructions
type Instr = Bit 32

-- | Register identifiers
type RegId = Bit 5

-- | Resume request to pipeline for multi-cycle instructions
data ResumeReq =
  ResumeReq {
    resumeReqData :: Bit 32
    -- ^ Data representing the result of the suspended operation
  , resumeReqDataTagBit :: Bit 1
    -- ^ Tag bit associated with data
  , resumeReqCap :: Option CapMemMeta
    -- ^ Capability meta-data for the result of the suspended operation
  } deriving (Generic, Interface, Bits)

-- | Pipeline state, visisble to the execute stage
data State =
  State {

    instr :: Bit 32
    -- ^ Current instruction

  , opA :: Bit 32
  , opB :: Bit 32
  , opBorImm :: Bit 32
  , immOrOpB :: Bit 32
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

  , suspend :: Action ()
    -- ^ Call this to implement a multi-cycle instruction.
    -- Results are returned via resume stage.

  , retry :: Action ()
    -- ^ Call this if the instruction cannot currently be executed
    -- (perhaps resources are not currently available).

  , opcode :: MnemonicVec
    -- ^ Mnemonic(s) for current instruction identified by the decoder

  , trap :: TrapCode -> Action ()
    -- ^ Call this to tigger an exception / interrupt

  -------------------
  -- CHERI support --
  -------------------

  , capA :: Cap
  , capB :: Cap
    -- ^ Capability operands.

  , pcc :: Cap
    -- ^ Read access to PCC.

  , pccNew :: WriteOnly CapPipe
    -- ^ Update the capability which is combined with the PC (via a
    -- call to setAddr) to derive the PCC.

  , resultCap :: WriteOnly CapPipe
    -- ^ Instruction result for capability reg file. The client should
    -- not write 'result' *and* 'resultCap' in the same clock cycle.

  } deriving (Generic, Interface)

-- | Register file id
type RegFileId = Bit 2
regFileIntCap :: RegFileId = 0
regFileInt :: RegFileId = 1
regFileCapMeta :: RegFileId = 2

-- | Upper bound on number of instruction mnemonics used by the decoder
type MaxMnemonics = 64

-- | Bit vector indentifying one or more active mnemonics
type MnemonicVec = Bit MaxMnemonics

-- | Interface to pipeline's execute stage
data ExecuteStage =
  ExecuteStage {
    execute :: Action ()
    -- ^ Trigger execute stage
  } deriving (Generic, Interface)
