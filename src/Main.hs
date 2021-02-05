-- Blarney imports
import Blarney

-- Pebbles imports
import Pebbles.SoC.Top

-- Generate code for scalar core
main :: IO ()
main = do
  writeVerilogModule makeTop "Pebbles" "./"
