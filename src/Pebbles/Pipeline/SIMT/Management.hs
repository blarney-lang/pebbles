module Pebbles.Pipeline.SIMT.Management where

-- Types for SIMT pipeline management

-- Blarney imports
import Blarney

-- | SIMT pipeline management commands
type SIMTCmd = Bit 1

-- | Write to tightly-coupled instruction memory
simtCmd_WriteInstr :: SIMTCmd = 0

-- | Start all warps with a given PC
simtCmd_StartPipeline :: SIMTCmd = 1

-- | SIMT pipeline management request (from CPU)
data SIMTReq =
  SIMTReq {
    simtReqCmd :: SIMTCmd
  , simtReqAddr :: Bit 32
  , simtReqData :: Bit 32
  } deriving (Generic, Bits)

-- | SIMT pipeline management response (to CPU)
type SIMTResp = SIMTReturnCode

-- True on success, false otherwise
type SIMTReturnCode = Bit 1
