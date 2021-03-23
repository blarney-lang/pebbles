module Pebbles.Instructions.Units.AddUnit where
  
-- Blarney imports
import Blarney

-- Interface
-- =========

data AddIns =
  AddIns {
    addUnsigned :: Bit 1
    -- ^ Are the operands unsigned?
  , addSub :: Bit 1
    -- ^ Do subtraction/comparison rather than addition?
  , addOpA :: Bit 32
  , addOpB :: Bit 32
  }

data AddOuts =
  AddOuts {
    addSum :: Bit 32
    -- ^ Output of adder
  , addLessThan :: Bit 1
    -- ^ Result of comparison (assuming addSub was true)
  , addEqual :: Bit 1
  }

-- Implementation
-- ==============

-- | Add/sub/compare unit
addUnit :: AddIns -> AddOuts
addUnit ins =
    AddOuts {
      addSum = truncate sum
    , addLessThan = at @32 sum
    , addEqual = a .==. b
    }
  where
    u = ins.addUnsigned
    a = ins.addOpA
    b = ins.addOpB
    isSub = ins.addSub
    addA = (if u then 0 else at @31 a) # a
    addB = (if u then 0 else at @31 b) # b
    sum = addA + (if isSub then inv addB else addB)
               + (if isSub then 1 else 0)
