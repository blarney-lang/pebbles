module CHERI.CapLib
  ( module CHERI.CapLibBase
  , splitCap
  , unsplitCap
  , maskValidBitMeta
  , nullCapVal
  , almightyCapVal
  , nullCapMetaVal
  , nullCapMetaInteger
  ) where

-- Blarney imports
import Blarney

-- CHERI imports
import CHERI.CapLibBase

-- Haskell imports
import qualified Data.Bits as B

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
