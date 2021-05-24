module Pebbles.SoC.DRAM.Interface where

-- SoC parameters
#include <SoC.h>

-- Blarney imports
import Blarney

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
