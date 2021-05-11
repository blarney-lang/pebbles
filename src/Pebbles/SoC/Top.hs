-- System-on-chip top-level Blarney module

module Pebbles.SoC.Top where

-- SoC parameters
#include <SoC.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.SourceSink
import Blarney.Connectable
import Blarney.Interconnect

-- Pebbles imports
import Pebbles.SoC.JTAGUART
import Pebbles.SoC.Core.SIMT
import Pebbles.SoC.Core.Scalar
import Pebbles.SoC.DRAM.DualPort
import Pebbles.SoC.DRAM.Interface
import Pebbles.Memory.SBDCache
import Pebbles.Memory.Alignment
import Pebbles.Memory.Interface
import Pebbles.Memory.BankedSRAMs
import Pebbles.Memory.WarpPreserver
import Pebbles.Memory.CoalescingUnit

-- SoC top-level interface
-- =======================

-- | SoC inputs
data SoCIns =
  SoCIns {
    -- | JTAG UART inputs
    socUARTIns :: AvalonJTAGUARTIns
    -- | DRAM inputs
  , socDRAMIns :: AvalonDRAMIns
  }
  deriving (Generic, Interface)

-- | SoC outputs
data SoCOuts =
  SoCOuts {
    -- | JTAG UART outputs
    socUARTOuts :: AvalonJTAGUARTOuts
    -- | DRAM outputs
  , socDRAMOuts :: AvalonDRAMOuts
  }
  deriving (Generic, Interface)

-- SoC top-level module
-- ====================

-- | SoC top-level
makeTop :: SoCIns -> Module SoCOuts
makeTop socIns = mdo
  -- Scalar core
  let cpuConfig =
        ScalarCoreConfig {
          scalarCoreInstrMemInitFile = Just "boot.mif"
        , scalarCoreInstrMemLogNumInstrs = CPUInstrMemLogWords
      }
  cpuOuts <- makeScalarCore cpuConfig
    ScalarCoreIns {
      scalarUartIn = fromUART
    , scalarMemUnit = cpuMemUnit
    , scalarSIMTResps = simtMgmtResps
    }

  -- Data cache
  (cpuMemUnit, dramReqs0) <- makeSBDCache dramResps0

  -- SIMT core
  let simtConfig =
        SIMTCoreConfig {
          simtCoreInstrMemInitFile = Nothing
        , simtCoreInstrMemLogNumInstrs = CPUInstrMemLogWords
        , simtCoreExecBoundary = True
        }
  simtMgmtResps <- makeSIMTCore simtConfig
    (cpuOuts.scalarSIMTReqs)
    simtMemUnits

  -- SIMT memory subsystem
  (simtMemUnits, dramReqs1) <- makeSIMTMemSubsystem dramResps1

  -- DRAM instance
  ((dramResps0, dramResps1), avlDRAMOuts) <-
    makeDRAMDualPort (dramReqs0, dramReqs1) (socIns.socDRAMIns)

  -- Avalon JTAG UART wrapper module
  (fromUART, avlUARTOuts) <- makeJTAGUART
    (cpuOuts.scalarUartOut)
    (socIns.socUARTIns)

  return
    SoCOuts {
      socUARTOuts = avlUARTOuts
    , socDRAMOuts = avlDRAMOuts
    }

-- SIMT memory subsystem
-- =====================

makeSIMTMemSubsystem :: Bits t_id =>
     -- | DRAM responses
     Stream (DRAMResp ())
     -- | DRAM requests and per-lane mem units
  -> Module ([MemUnit t_id], Stream (DRAMReq ()))
makeSIMTMemSubsystem dramResps = mdo
    -- Warp preserver
    (memReqs, simtMemUnits) <- makeWarpPreserver memResps1

    -- Prepare request for memory subsystem
    let prepareReq req =
          req {
            -- Align store-data (account for access width)
            memReqData =
              writeAlign (req.memReqAccessWidth) (req.memReqData)
            -- Remember info needed to process response
          , memReqId =
              ( req.memReqId
              , MemReqInfo {
                  memReqInfoAddr = req.memReqAddr.truncate
                , memReqInfoAccessWidth = req.memReqAccessWidth
                , memReqInfoIsUnsigned = req.memReqIsUnsigned
                }
              )
          }
    let memReqs1 = map (mapSource prepareReq) memReqs

    -- Coalescing unit
    (memResps, dramReqs) <-
      makeCoalescingUnit isBankedSRAMAccess memReqs1 dramResps

    -- Process response from memory subsystem
    let processResp resp =
          resp {
            -- | Drop info, no longer needed
            memRespId = resp.memRespId.fst
            -- | Use info to mux loaded data
          , memRespData = loadMux (resp.memRespData)
              (resp.memRespId.snd.memReqInfoAddr.truncate)
              (resp.memRespId.snd.memReqInfoAccessWidth)
              (resp.memRespId.snd.memReqInfoIsUnsigned)
          }
    let memResps1 = map (mapSource processResp) memResps

    -- Ensure that the SRAM base address is suitably aligned
    -- (If so, remapping SRAM addresses is unecessary)
    if sramBase `mod` sramSize /= 0
      then error "SRAM base address not suitably aligned"
      else return ()

    return (simtMemUnits, dramReqs)

  where
    -- SRAM-related addresses
    simtStacksStart = 2 ^ (DRAMAddrWidth + DRAMBeatLogBytes) -
      2 ^ (SIMTLogLanes + SIMTLogWarps + SIMTLogBytesPerStack)
    sramSize = 2 ^ (SIMTLogLanes + SIMTLogWordsPerSRAMBank+2)
    sramBase = simtStacksStart - sramSize

    -- Determine if request maps to banked SRAMs
    -- (Local fence goes to banked SRAMs)
    isBankedSRAMAccess :: MemReq t_id -> Bit 1
    isBankedSRAMAccess req =
      req.memReqOp .==. memLocalFenceOp .||.
        (req.memReqOp .!=. memGlobalFenceOp .&&.
           addr .<. fromInteger simtStacksStart .&&.
             addr .>=. fromInteger sramBase)
      where addr = req.memReqAddr
