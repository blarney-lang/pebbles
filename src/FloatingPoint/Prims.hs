module FloatingPoint.Prims where

-- Blarney imports
import Blarney
import Blarney.Core.BV

fpAddSub :: Bit 1 -> Bit 32 -> Bit 32 -> Bit 32
fpAddSub sel a b = FromBV q
  where
    custom =
      Custom {
        customName      = "FPAddSubWrapper"
      , customInputs    = [("opSel", 1), ("a", 32), ("b", 32)]
      , customOutputs   = [("q", 32)]
      , customParams    = []
      , customIsClocked = True
      , customResetable = True
      , customNetlist   = Nothing
      }
    [q] = makePrim custom [toBV sel, toBV a, toBV b]
                          [Just "q"]

fpBinOp :: String -> Bit 32 -> Bit 32 -> Bit 32
fpBinOp op a b = FromBV q
  where
    custom =
      Custom {
        customName      = op
      , customInputs    = [("a", 32), ("b", 32)]
      , customOutputs   = [("q", 32)]
      , customParams    = []
      , customIsClocked = True
      , customResetable = True
      , customNetlist   = Nothing
      }
    [q] = makePrim custom [toBV a, toBV b]
                          [Just "q"]

fpMin :: Bit 32 -> Bit 32 -> Bit 32
fpMin = fpBinOp "FPMinWrapper"

fpMax :: Bit 32 -> Bit 32 -> Bit 32
fpMax = fpBinOp "FPMaxWrapper"

fpMul :: Bit 32 -> Bit 32 -> Bit 32
fpMul = fpBinOp "FPMulWrapper"

fpDiv :: Bit 32 -> Bit 32 -> Bit 32
fpDiv = fpBinOp "FPDivWrapper"

fpUnOp :: String -> Bit 32 -> Bit 32
fpUnOp op a = FromBV q
  where
    custom =
      Custom {
        customName      = op
      , customInputs    = [("a", 32)]
      , customOutputs   = [("q", 32)]
      , customParams    = []
      , customIsClocked = True
      , customResetable = True
      , customNetlist   = Nothing
      }
    [q] = makePrim custom [toBV a]
                          [Just "q"]

fpSqrt :: Bit 32 -> Bit 32
fpSqrt = fpUnOp "FPSqrtWrapper"

fpToInt :: Bit 32 -> Bit 32
fpToInt = fpUnOp "FPToIntWrapper"

fpToUInt :: Bit 32 -> Bit 32
fpToUInt = fpUnOp "FPToUIntWrapper"

fpFromInt :: Bit 32 -> Bit 32
fpFromInt = fpUnOp "FPFromIntWrapper"

fpFromUInt :: Bit 32 -> Bit 32
fpFromUInt = fpUnOp "FPFromUIntWrapper"

fpCompare :: String -> Bit 32 -> Bit 32 -> Bit 1
fpCompare cmp a b = FromBV q
  where
    custom =
      Custom {
        customName      = cmp
      , customInputs    = [("a", 32), ("b", 32)]
      , customOutputs   = [("q", 1)]
      , customParams    = []
      , customIsClocked = True
      , customResetable = True
      , customNetlist   = Nothing
      }
    [q] = makePrim custom [toBV a, toBV b]
                          [Just "q"]

fpCompareEq :: Bit 32 -> Bit 32 -> Bit 1
fpCompareEq = fpCompare "FPCompareEqWrapper"

fpCompareLT :: Bit 32 -> Bit 32 -> Bit 1
fpCompareLT = fpCompare "FPCompareLTWrapper"

fpCompareLTE :: Bit 32 -> Bit 32 -> Bit 1
fpCompareLTE = fpCompare "FPCompareLTEWrapper"

fpToInt33 :: Bit 32 -> Bit 33
fpToInt33 a = FromBV q
  where
    custom =
      Custom {
        customName      = "FPToInt33Wrapper"
      , customInputs    = [("a", 32)]
      , customOutputs   = [("q", 33)]
      , customParams    = []
      , customIsClocked = True
      , customResetable = True
      , customNetlist   = Nothing
      }
    [q] = makePrim custom [toBV a]
                          [Just "q"]

fpFromInt33 :: Bit 33 -> Bit 32
fpFromInt33 a = FromBV q
  where
    custom =
      Custom {
        customName      = "FPFromInt33Wrapper"
      , customInputs    = [("a", 33)]
      , customOutputs   = [("q", 32)]
      , customParams    = []
      , customIsClocked = True
      , customResetable = True
      , customNetlist   = Nothing
      }
    [q] = makePrim custom [toBV a]
                          [Just "q"]
