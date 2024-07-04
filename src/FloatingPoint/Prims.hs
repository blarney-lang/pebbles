module FloatingPoint.Prims where

-- Blarney imports
import Blarney
import Blarney.Core.BV

fpAddSub :: Int -> Bit 1 -> Bit 32 -> Bit 32 -> Bit 32
fpAddSub latency sel a b = FromBV q
  where
    custom =
      Custom {
        customName      = "FPAddSubWrapper"
      , customInputs    = [("opSel", 1), ("a", 32), ("b", 32)]
      , customOutputs   = [("q", 32)]
      , customParams    = ["LATENCY" :-> show latency]
      , customIsClocked = True
      , customResetable = True
      , customNetlist   = Nothing
      }
    [q] = makePrim custom [toBV sel, toBV a, toBV b]
                          [Just "q"]

fpBinOp :: String -> Int -> Bit 32 -> Bit 32 -> Bit 32
fpBinOp op latency a b = FromBV q
  where
    custom =
      Custom {
        customName      = op
      , customInputs    = [("a", 32), ("b", 32)]
      , customOutputs   = [("q", 32)]
      , customParams    = ["LATENCY" :-> show latency]
      , customIsClocked = True
      , customResetable = True
      , customNetlist   = Nothing
      }
    [q] = makePrim custom [toBV a, toBV b]
                          [Just "q"]

fpMin :: Int -> Bit 32 -> Bit 32 -> Bit 32
fpMin = fpBinOp "FPMinWrapper"

fpMax :: Int -> Bit 32 -> Bit 32 -> Bit 32
fpMax = fpBinOp "FPMaxWrapper" 

fpMul :: Int -> Bit 32 -> Bit 32 -> Bit 32
fpMul = fpBinOp "FPMulWrapper"

fpDiv :: Int -> Bit 32 -> Bit 32 -> Bit 32
fpDiv = fpBinOp "FPDivWrapper"

fpUnOp :: String -> Int -> Bit 32 -> Bit 32
fpUnOp op latency a = FromBV q
  where
    custom =
      Custom {
        customName      = op
      , customInputs    = [("a", 32)]
      , customOutputs   = [("q", 32)]
      , customParams    = ["LATENCY" :-> show latency]
      , customIsClocked = True
      , customResetable = True
      , customNetlist   = Nothing
      }
    [q] = makePrim custom [toBV a]
                          [Just "q"]

fpSqrt :: Int -> Bit 32 -> Bit 32
fpSqrt = fpUnOp "FPSqrtWrapper"

fpToInt :: Int -> Bit 32 -> Bit 32
fpToInt = fpUnOp "FPToIntWrapper"

fpToUInt :: Int -> Bit 32 -> Bit 32
fpToUInt = fpUnOp "FPToUIntWrapper"

fpFromInt :: Int -> Bit 32 -> Bit 32
fpFromInt = fpUnOp "FPFromIntWrapper"

fpFromUInt :: Int -> Bit 32 -> Bit 32
fpFromUInt = fpUnOp "FPFromUIntWrapper"

fpCompare :: String -> Int -> Bit 32 -> Bit 32 -> Bit 1
fpCompare cmp latency a b = FromBV q
  where
    custom =
      Custom {
        customName      = cmp
      , customInputs    = [("a", 32), ("b", 32)]
      , customOutputs   = [("q", 1)]
      , customParams    = ["LATENCY" :-> show latency]
      , customIsClocked = True
      , customResetable = True
      , customNetlist   = Nothing
      }
    [q] = makePrim custom [toBV a, toBV b]
                          [Just "q"]

fpCompareEq :: Int -> Bit 32 -> Bit 32 -> Bit 1
fpCompareEq = fpCompare "FPCompareEqWrapper"

fpCompareLT :: Int -> Bit 32 -> Bit 32 -> Bit 1
fpCompareLT = fpCompare "FPCompareLTWrapper"

fpCompareLTE :: Int -> Bit 32 -> Bit 32 -> Bit 1
fpCompareLTE = fpCompare "FPCompareLTEWrapper"

fpToInt33 :: Int -> Bit 32 -> Bit 33
fpToInt33 latency a = FromBV q
  where
    custom =
      Custom {
        customName      = "FPToInt33Wrapper"
      , customInputs    = [("a", 32)]
      , customOutputs   = [("q", 33)]
      , customParams    = ["LATENCY" :-> show latency]
      , customIsClocked = True
      , customResetable = True
      , customNetlist   = Nothing
      }
    [q] = makePrim custom [toBV a]
                          [Just "q"]

fpFromInt33 :: Int -> Bit 33 -> Bit 32
fpFromInt33 latency a = FromBV q
  where
    custom =
      Custom {
        customName      = "FPFromInt33Wrapper"
      , customInputs    = [("a", 33)]
      , customOutputs   = [("q", 32)]
      , customParams    = ["LATENCY" :-> show latency]
      , customIsClocked = True
      , customResetable = True
      , customNetlist   = Nothing
      }
    [q] = makePrim custom [toBV a]
                          [Just "q"]
