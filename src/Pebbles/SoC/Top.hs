-- System-on-chip top-level Blarney module

module Pebbles.SoC.Top where

-- SoC parameters
#include <SoC.h>

-- Blarney imports
import Blarney
import Blarney.Stream

-- Pebbles imports
import Pebbles.SoC.JTAGUART
import Pebbles.SoC.Core.Scalar
import Pebbles.SoC.DRAM.Wrapper
import Pebbles.SoC.DRAM.Interface
import Pebbles.Memory.SBDCache
import Pebbles.Memory.Interface

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
  let coreConfig =
        ScalarCoreConfig {
          scalarCoreInstrMemInitFile = Just "boot.mif"
        , scalarCoreInstrMemLogNumInstrs = CPUInstrMemLogWords
      }
  toUART <- makeScalarCore coreConfig fromUART memUnit

  -- Data cache
  (memUnit, dramReqs) <- makeSBDCache dramResps

  -- DRAM Avalon wrapper module
  (dramResps, avlDRAMOuts) <- makeDRAM dramReqs (socIns.socDRAMIns)

  -- Avalon JTAG UART wrapper module
  (fromUART, avlUARTOuts) <- makeJTAGUART toUART (socIns.socUARTIns)

  return
    SoCOuts {
      socUARTOuts = avlUARTOuts
    , socDRAMOuts = avlDRAMOuts
    }
