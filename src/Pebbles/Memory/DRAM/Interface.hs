module Pebbles.Memory.DRAM.Interface where

-- SoC parameters
#include <Config.h>

-- Blarney imports
import Blarney
import qualified Blarney.Vector as V

-- DRAM interface
-- ==============

type DRAMAddr   = Bit DRAMAddrWidth
type DRAMBeat   = Bit DRAMBeatBits
type DRAMBurst  = Bit DRAMBurstWidth
type DRAMByteEn = Bit DRAMBeatBytes

-- | DRAM request
data DRAMReq id = 
  DRAMReq {
    dramReqId :: id
    -- ^ Request id
  , dramReqIsStore :: Bit 1
    -- ^ Store operation?
  , dramReqAddr :: DRAMAddr
    -- ^ Address
  , dramReqData :: DRAMBeat
    -- ^ Data to store
  , dramReqDataTagBits :: Bit DRAMBeatWords
    -- ^ Data tag bit per word (see note [Density of tag bits])
  , dramReqByteEn :: DRAMByteEn
    -- ^ Byte enable of store
  , dramReqBurst :: DRAMBurst
    -- ^ Burst
  , dramReqIsFinal :: Bit 1
    -- ^ Is this the final request in an atomic transaction?
  } deriving (Generic, FShow, Bits, Interface)

-- | DRAM response
data DRAMResp id =
  DRAMResp {
    dramRespId :: id
    -- ^ Response id
  , dramRespBurstId :: DRAMBurst
    -- ^ Beat id for burst
  , dramRespData :: DRAMBeat
    -- ^ Result of load operation
  , dramRespDataTagBits :: Bit DRAMBeatWords
    -- ^ Data tag bit per word (see note [Density of tag bits])
  } deriving (Generic, FShow, Bits, Interface)

-- Avalon DRAM interface
-- =====================

-- | Inputs coming from Avalon
data AvalonDRAMIns =
  AvalonDRAMIns {
    avl_dram_readdata      :: DRAMBeat
  , avl_dram_readdatavalid :: Bit 1
  , avl_dram_waitrequest   :: Bit 1
  } deriving (Generic, Bits, Interface)

-- | Outputs going to Avalon
data AvalonDRAMOuts =
  AvalonDRAMOuts {
    avl_dram_read       :: Bit 1
  , avl_dram_write      :: Bit 1
  , avl_dram_writedata  :: DRAMBeat
  , avl_dram_address    :: DRAMAddr
  , avl_dram_byteen     :: DRAMByteEn
  , avl_dram_burstcount :: DRAMBurst  
  } deriving (Generic, Bits, Interface)

-- Note [Density of tag bits]
-- ==========================

-- DRAM requests and responses contain one tag bit for each 32-bit
-- word in a beat.  The memory subsystem maintains the invariant that
-- all tag bits of a capability are either all-one or all-zero.  So
-- why not just use one tag bit per capability?  Because SIMT stack
-- interleaving means that the words of a capability on a thread's
-- stack are not contiguous in physical memory; therefore each word in
-- a DRAM beat could be from a different capability.  Multiple tag
-- bits for a capability are never actually stored
-- separately/redundantly in memory.

-- | Given a tag bit per word, determine tag bit per capability
toTagBits :: Bit DRAMBeatWords -> Bit TagBitsPerBeat
toTagBits bits = pack $ V.map headBit $ unpack bits
  where headBit v = V.head v :: Bit 1

-- | Given a tag bit per capability, determine tag bit per word
fromTagBits :: Bit TagBitsPerBeat -> Bit DRAMBeatWords
fromTagBits bits = pack $ V.map V.replicate v
  where v :: V.Vec TagBitsPerBeat (Bit 1) = unpack bits
