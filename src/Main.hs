-- Blarney imports
import Blarney

-- Pebbles imports
import Pebbles.SoC.Top
import Pebbles.SoC.Core.SIMT

-- Generate code for scalar core
main :: IO ()
main = do
  writeVerilogModule makeTop "Pebbles" "./"
  genSIMTExecuteStage "./"
