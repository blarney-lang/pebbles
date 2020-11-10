-- Blarney imports
import Blarney
import Blarney.Stream
import Blarney.SourceSink

-- Pebbles imports
import Pebbles.Core.Scalar

-- Generate code for scalar core
main :: IO ()
main = do
  -- Simulation version
  let simConfig =
        ScalarCoreConfig {
          scalarCoreInstrMemInitFile     = Just "prog.hex"
        , scalarCoreDataMemInitFile      = Just "data.hex"
        , scalarCoreDataMemLogNumWords   = 14
        , scalarCoreInstrMemLogNumInstrs = 14
        }
  writeVerilogModule (makeScalarCoreSim simConfig) "SimPebbles" "../sim/"

  -- Synthesis version
  let config =
        ScalarCoreConfig {
          scalarCoreInstrMemInitFile   = Just "prog.mif"
        , scalarCoreDataMemInitFile    = Just "data.mif"
        , scalarCoreDataMemLogNumWords = 14
        , scalarCoreInstrMemLogNumInstrs = 14
        }
  writeVerilogModule (makeScalarCore config) "Pebbles" "./"
