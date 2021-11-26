-- SIMT register file implementations
module Pebbles.Pipeline.SIMT.RegFile where

-- SoC configuration
#include <Config.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.Stream
import Blarney.PulseWire
import Blarney.QuadPortRAM
import Blarney.Interconnect
import Blarney.Vector (Vec, fromList, toList)

-- Pebbles imports
import Pebbles.Pipeline.Interface

-- | Register file index
type SIMTRegFileIdx = (Bit SIMTLogWarps, RegId)

-- | Vector register file
data SIMTRegFile t_reg =
  SIMTRegFile {
    loadA :: SIMTRegFileIdx -> Action ()
    -- ^ Issue load for given register on port A
  , loadB :: SIMTRegFileIdx -> Action ()
    -- ^ Issue load for given register on port B
  , outA :: Vec SIMTLanes t_reg
    -- ^ Port A's value available one cycle after load
  , outB :: Vec SIMTLanes t_reg
    -- ^ Port B's value available one cycle after load
  , store :: SIMTRegFileIdx
          -> Vec SIMTLanes (Option t_reg)
          -> Action ()
    -- ^ Write register
  }

-- | Null implementation
makeNullSIMTRegFile :: Bits t_reg => Module (SIMTRegFile t_reg)
makeNullSIMTRegFile = do
  return
    SIMTRegFile {
      loadA = \_ -> return ()
    , loadB = \_ -> return ()
    , outA = dontCare
    , outB = dontCare
    , store = \_ _ -> return ()
    }

-- | Simple implemenation
makeSimpleSIMTRegFile :: Bits t_reg =>
     Maybe String
     -- ^ Optional initialisation file
  -> Module (SIMTRegFile t_reg)
makeSimpleSIMTRegFile initFile = do

  -- Register file banks, one per lane
  (banksA, banksB) ::
    ([RAM SIMTRegFileIdx t_reg],
     [RAM SIMTRegFileIdx t_reg]) <-
       unzip <$> replicateM SIMTLanes (makeQuadRAMCore initFile)

  return
    SIMTRegFile {
      loadA = \idx -> sequence_ [bank.load idx | bank <- banksA]
    , loadB = \idx -> sequence_ [bank.load idx | bank <- banksB]
    , outA = fromList [bank.out | bank <- banksA]
    , outB = fromList [bank.out | bank <- banksB]
    , store = \idx vec -> do
        sequence_
          [ when item.valid do bank.store idx item.val
          | (item, bank) <- zip (toList vec) banksA ]
    }
