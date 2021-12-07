{-# LANGUAGE UndecidableInstances   #-}

module Pebbles.Util.Either
  ( (:|:)
  , makeLeft
  , makeRight
  , isLeft
  , isRight
  , getLeft
  , getRight
  ) where

-- Blarney imports
import Blarney
import Blarney.Core.BV

-- GHC imports
import GHC.Types
import Data.Type.Bool
import Data.Type.Equality

-- | Either type (abstract)
data a :|: b = Either BV BV

-- | Bottom value, used internally
bottom :: a
bottom = error "Pebbles.Util.Either.bottom"

-- | Bits instance for either type
instance (Bits t, Bits u) => Bits (t :|: u) where
  type SizeOf (t :|: u) = 1 + Max (SizeOf t) (SizeOf u)
  sizeOf _ = 1 + (sizeOf (bottom :: t) `max` sizeOf (bottom :: u))
  pack (Either sel val) = FromBV (sel `concatBV` val)
  unpack inp = Either (selectBV (w, w) bv)
                      (selectBV (w-1, 0) bv)
    where
      bv = toBV inp
      w = sizeOf (bottom :: t) `max` sizeOf (bottom :: u)
  nameBits nm (Either sel val) =
    Either (addBVNameHint sel (NmRoot 0 ("usel_" ++ nm)))
           (addBVNameHint val (NmRoot 0 ("uval_" ++ nm)))

makeLeft :: forall t u. (Bits t, Bits u) => t -> (t :|: u)
makeLeft x = Either (constBV 1 0) (zeroExtendBV w (toBV (pack x)))
  where w = sizeOf (bottom :: t) `max` sizeOf (bottom :: u)

makeRight :: forall t u. (Bits t, Bits u) => t -> (u :|: t)
makeRight x = Either (constBV 1 1) (zeroExtendBV w (toBV (pack x)))
  where w = sizeOf (bottom :: t) `max` sizeOf (bottom :: u)

isRight :: (t :|: u) -> Bit 1
isRight (Either sel val) = FromBV sel

isLeft :: (t :|: u) -> Bit 1
isLeft x = inv (isRight x)

getLeft :: forall t u. Bits t => (t :|: u) -> t
getLeft (Either sel val) = unpack (FromBV (selectBV (w-1, 0) val))
  where w = sizeOf (bottom :: t)

getRight :: forall t u. Bits u => (t :|: u) -> u
getRight (Either sel val) = unpack (FromBV (selectBV (w-1, 0) val))
  where w = sizeOf (bottom :: u)

-- | Type function for computing the max of two type-level nats
type family Max (a :: Nat) (b :: Nat) :: Nat where
  Max a b = If (CmpNat a b == GT) a b
