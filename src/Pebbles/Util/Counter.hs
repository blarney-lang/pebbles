module Pebbles.Util.Counter where

import Blarney

-- | N-bit counter interface
data Counter n =
  Counter {
    incrBy       :: Bit n -> Action ()
  , decrBy       :: Bit n -> Action ()
  , isFull       :: Bit 1
  , getAvailable :: Bit n
  , getCount     :: Bit n
  }

-- | N-bit counter with parallel increment/decrement support
makeCounter :: KnownNat n => Bit n -> Module (Counter n)
makeCounter maxVal =
  do incrWire :: Wire (Bit n) <- makeWire 0
     decrWire :: Wire (Bit n) <- makeWire 0
     countReg :: Reg (Bit n) <- makeReg 0
     fullReg :: Reg (Bit 1) <- makeReg 0

     always do
       let newCount :: Bit n = (countReg.val + incrWire.val) - decrWire.val
       countReg <== newCount
       fullReg <== newCount .==. maxVal

     return $ Counter {
       incrBy = (incrWire <==)
     , decrBy = (decrWire <==)
     , isFull = fullReg.val
     , getCount = countReg.val
     , getAvailable = maxVal - countReg.val
     }
