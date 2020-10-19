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
          scalarCoreInstrMemInitFile = Just "prog.hex"
        , scalarCoreDataMemInitFile  = Just "data.hex"
        }
  writeVerilogModule (makeScalarCoreSim simConfig) "SimPebbles" "../sim/"

  -- Synthesis version
  let config =
        ScalarCoreConfig {
          scalarCoreInstrMemInitFile = Just "prog.mif"
        , scalarCoreDataMemInitFile  = Just "data.mif"
        }
  writeVerilogModule (makeScalarCore config) "Pebbles" "./"
