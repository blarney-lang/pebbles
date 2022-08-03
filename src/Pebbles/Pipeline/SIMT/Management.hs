module Pebbles.Pipeline.SIMT.Management where

-- Types for SIMT pipeline management

-- Blarney imports
import Blarney

-- | SIMT pipeline management commands
data SIMTCmd = SIMTCmd (Bit 2)
  deriving (Generic, Interface, Bits, Cmp)

-- | Write to tightly-coupled instruction memory
simtCmd_WriteInstr = SIMTCmd 0

-- | Start all warps with a given PC
simtCmd_StartPipeline = SIMTCmd 1

-- | Set number of warps per block
simtCmd_SetWarpsPerBlock = SIMTCmd 2

-- | Read stats from SIMT core
simtCmd_AskStats = SIMTCmd 3

-- | Id of a performance stat counter
data SIMTStatId = SIMTStatId (Bit 4)
  deriving (Generic, Interface, Bits, Cmp)

-- | Cycle count
simtStat_Cycles = SIMTStatId 0

-- | Instruction count
simtStat_Instrs = SIMTStatId 1

-- | Vector register count
simtStat_VecRegs = SIMTStatId 2

-- | Capability vector register count
simtStat_CapVecRegs = SIMTStatId 3

-- | Count scalarisable instructions (when scalar unit disabled)
-- and scalarised instructions (when scalar unit enabled)
simtStat_ScalarisableInstrs = SIMTStatId 4

-- | Vector pipeline retry bubbles
simtStat_Retries = SIMTStatId 5

-- | Vector pipeline suspension bubbles
simtStat_SuspBubbles = SIMTStatId 6

-- | Scalar pipeline suspension bubbles
simtStat_ScalarSuspBubbles = SIMTStatId 7

-- | Scalar pipeline abort bubbles
simtStat_ScalarAborts = SIMTStatId 8

-- | DRAM accesses (loads and stores)
simtStat_DRAMAccesses = SIMTStatId 9

-- | SIMT pipeline management request (from CPU)
data SIMTReq =
  SIMTReq {
    simtReqCmd :: SIMTCmd
  , simtReqAddr :: Bit 32
  , simtReqData :: Bit 32
  } deriving (Generic, Interface, Bits)

-- | SIMT pipeline management response (to CPU)
type SIMTResp = Bit 32

-- | Kernel exit code
type SIMTExitCode = Bit 2

-- | Successfull exit from software
simtExit_Success :: SIMTExitCode = 0

-- | Failed exit from software
simtExit_Failure :: SIMTExitCode = 1

-- | Exception raised
simtExit_Exception :: SIMTExitCode = 2
