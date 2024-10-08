module CHERI.CapLib
  ( module CHERI.CapLibBase
  , splitCapPipe
  , unsplitCapPipe
  , nullCapMemVal
  , almightyCapMemVal
  , almightyCapMemMetaVal
  , nullCapMemMetaVal
  , nullCapMemMetaInteger
  , Cap(..)
  , decodeCapPipe
  , decodeCapPipe'
  , decodeCapMem
  , almightyCapPipeVal
  , nullCapPipeVal
  , nullCapPipeMetaVal
  , nullCapPipeMetaInteger
  ) where

-- Blarney imports
import Blarney

-- CHERI imports
import CHERI.CapLibBase

-- Haskell imports
import qualified Data.Bits as B

-- Helper functions on CapPipe
-- ===========================

-- | Split capability into meta-data and address
splitCapPipe :: CapPipe -> (CapPipeMeta, CapAddr)
splitCapPipe cap = (isValidCap cap # lower cap, getAddr cap)

-- | Make capability from meta-data and address
unsplitCapPipe :: (CapPipeMeta, CapAddr) -> CapPipe
unsplitCapPipe (meta, addr) = tag # addr # lower meta
  where tag = upper meta :: Bit 1

-- | Direct almighty capability (not via Verilog)
almightyCapPipeVal :: CapPipe
almightyCapPipeVal = fromInteger almightyCapPipeInteger

-- | Direct null capability (not via Verilog)
nullCapPipeVal :: CapPipe
nullCapPipeVal = fromInteger nullCapPipeInteger

-- | Null capability meta-data
nullCapPipeMetaVal :: CapPipeMeta
nullCapPipeMetaVal = upper nullCapPipeVal

-- | Null capability meta-data as an integer
nullCapPipeMetaInteger :: Integer
nullCapPipeMetaInteger = nullCapPipeInteger B..&. mask
  where
    mw = valueOf @CapPipeMetaWidth
    mask = (1 `B.shiftL` (mw-1)) - 1

-- Helper functions on CapMem
-- ==========================

-- | Direct almighty capability (not via Verilog)
almightyCapMemVal :: CapMem
almightyCapMemVal = fromInteger almightyCapMemInteger

-- | Almighty capability meta-data
almightyCapMemMetaVal :: CapMemMeta
almightyCapMemMetaVal = upper almightyCapMemVal

-- | Direct null capability (not via Verilog)
nullCapMemVal :: CapMem
nullCapMemVal = fromInteger nullCapMemInteger

-- | Null capability meta-data
nullCapMemMetaVal :: CapMemMeta
nullCapMemMetaVal = upper nullCapMemVal

-- | Null capability meta-data as an integer
nullCapMemMetaInteger :: Integer
nullCapMemMetaInteger = nullCapMemInteger `B.shiftR` valueOf @CapAddrWidth

-- Partially-decoded capabilities
-- ==============================

-- | Partially decoded capability
data Cap =
  Cap {
      capPipe     :: CapPipe
    , capBase     :: CapAddr
    , capLength   :: Bit (CapAddrWidth+1)
    , capTop      :: Bit (CapAddrWidth+1)
    , capMem      :: CapMem
  }
  deriving (Generic, Interface, Bits)

-- | Partially decode given capability
decodeCapPipe' :: CapMem -> CapPipe -> Cap
decodeCapPipe' cm c =
  Cap {
      capPipe    = c
    , capBase    = base
    , capLength  = len
    , capTop     = zeroExtend base + len
    , capMem     = cm
  }
  where
    info = getBoundsInfo c
    len  = info.length
    base = info.base

decodeCapPipe :: CapPipe -> Cap
decodeCapPipe c = decodeCapPipe' dontCare c

-- | Partially decode given capability
decodeCapMem :: CapMem -> Cap
decodeCapMem cm = decodeCapPipe' cm (fromMem $ unpack cm)
