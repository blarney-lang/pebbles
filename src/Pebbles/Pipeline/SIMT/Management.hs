module Pebbles.Pipeline.SIMT.Management where

-- Types for SIMT pipeline management

-- Blarney imports
import Blarney

-- | SIMT pipeline management commands
data SIMTCmd = SIMTCmd (Bit 2)
  deriving (Generic, Bits, Cmp)

-- | Write to tightly-coupled instruction memory
simtCmd_WriteInstr = SIMTCmd 0

-- | Start all warps with a given PC
simtCmd_StartPipeline = SIMTCmd 1

-- | Set number of warps per block
simtCmd_SetWarpsPerBlock = SIMTCmd 2

-- | Read stats from SIMT core
simtCmd_AskStats = SIMTCmd 3

-- | Id of a performance stat counter
data SIMTStatId = SIMTStatId (Bit 1)
  deriving (Generic, Bits, Cmp)

-- | Cycle count
simtStat_Cycles = SIMTStatId 0

-- | Instruction count
simtStat_Instrs = SIMTStatId 1

-- | SIMT pipeline management request (from CPU)
data SIMTReq =
  SIMTReq {
    simtReqCmd :: SIMTCmd
  , simtReqAddr :: Bit 32
  , simtReqData :: Bit 32
  } deriving (Generic, Bits)

-- | SIMT pipeline management response (to CPU)
type SIMTResp = Bit 32
