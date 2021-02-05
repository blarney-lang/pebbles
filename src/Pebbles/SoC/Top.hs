-- System-on-chip top-level Blarney module

module Pebbles.SoC.Top where

-- SoC parameters
#include <SoC.h>

-- Blarney imports
import Blarney
import Blarney.Stream

-- Pebbles imports
import Pebbles.SoC.JTAGUART
import Pebbles.SoC.Core.SIMT
import Pebbles.SoC.Core.Scalar
import Pebbles.SoC.DRAM.DualPort
import Pebbles.SoC.DRAM.Interface
import Pebbles.Memory.SBDCache
import Pebbles.Memory.Interface
import Pebbles.Memory.WarpPreserver
import Pebbles.Memory.CoalescingUnit

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

-- | Blarney SoC top-level
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
        , simtCoreExecBoundary = False
        }
  simtMgmtResps <- makeSIMTCore simtConfig
    (cpuOuts.scalarSIMTReqs)
    simtMemUnits'

  -- Coalescing unit
  (simtMemUnits, dramReqs1) <- makeCoalescingUnit dramResps1

  -- Warp preserver
  simtMemUnits' <- makeWarpPreserver simtMemUnits

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
