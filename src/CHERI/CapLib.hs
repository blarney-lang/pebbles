module CHERI.CapLib
  ( module CHERI.CapLibBase
  , splitCap
  , unsplitCap
  , maskValidBitMeta
  , nullCapVal
  , almightyCapVal
  , almightyCapMetaVal
  , nullCapMetaVal
  , nullCapMetaInteger
  , Cap(..)
  , decodeCap
  ) where

-- Blarney imports
import Blarney

-- CHERI imports
import CHERI.CapLibBase

-- Haskell imports
import qualified Data.Bits as B

-- Helper functions on capabilities
-- ================================

-- | Split capability into meta-data and address
splitCap :: InternalCap -> (InternalCapMetaData, Bit 32)
splitCap cap = (isValidCap cap # lower cap, getAddr cap)

-- | Make capability from meta-data and address
unsplitCap :: (InternalCapMetaData, Bit 32) -> InternalCap
unsplitCap (meta, addr) = tag # addr # lower meta
  where tag = upper meta :: Bit 1

-- | Apply mask to valid bit of meta data
maskValidBitMeta :: Bit 1 -> InternalCapMetaData -> InternalCapMetaData
maskValidBitMeta mask meta = (mask .&&. tag) # lower meta
  where tag = upper meta :: Bit 1

-- | Direct almighty capability (not via Verilog)
almightyCapVal :: InternalCap
almightyCapVal = fromInteger almightyCapInteger

-- | Almighty capability meta-data
almightyCapMetaVal :: InternalCapMetaData
almightyCapMetaVal = fst (splitCap almightyCapVal)

-- | Direct null capability (not via Verilog)
nullCapVal :: InternalCap
nullCapVal = fromInteger nullCapInteger

-- | Null capability meta-data
nullCapMetaVal :: InternalCapMetaData
nullCapMetaVal = fst (splitCap nullCapVal)

-- | Null capability meta-data as an integer
nullCapMetaInteger :: Integer
nullCapMetaInteger = nullCapInteger B..&. mask
  where
    cw = valueOf @InternalCapWidth
    mw = valueOf @InternalCapMetaDataWidth
    mask = (1 `B.shiftL` (mw-1)) - 1

-- Partially-decoded capabilities
-- ==============================

-- | Partially decoded capability
data Cap =
  Cap {
      capInternal :: InternalCap
    , capBase     :: Bit 32
    , capOffset   :: Bit 32
    , capLength   :: Bit 33
    , capTop      :: Bit 33
  }
  deriving (Generic, Interface, Bits)

-- | Partially decode given capability
decodeCap :: InternalCap -> Cap
decodeCap c =
  Cap {
      capInternal = c
    , capBase     = base
    , capOffset   = getAddr c - base
    , capLength   = len
    , capTop      = zeroExtend base + len
  }
  where
    len = getLength c
    base = getBase c
