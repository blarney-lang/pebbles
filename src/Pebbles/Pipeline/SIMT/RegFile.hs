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

-- | Per-warp register file index
type SIMTRegFileIdx = (Bit SIMTLogWarps, RegId)

-- | Vector register file
data SIMTRegFile t_reg =
  SIMTRegFile {
    loadA :: SIMTRegFileIdx -> Action ()
    -- ^ Issue load for given register on port A
  , loadB :: SIMTRegFileIdx -> Action ()
    -- ^ Issue load for given register on port B
  , outA :: Vec SIMTLanes t_reg
    -- ^ Port A's value available two cycles after load
  , outB :: Vec SIMTLanes t_reg
    -- ^ Port B's value available two cycles after load
  , store :: SIMTRegFileIdx
          -> Vec SIMTLanes (Option t_reg)
          -> Action ()
    -- ^ Write register
  , storeLatency :: Int
    -- ^ Number of cycles after 'store' before write is performed
  , init :: Action ()
    -- ^ Trigger initialisation of register file
  , initInProgress :: Bit 1
    -- ^ Don't trigger intialisation while it is already in progress
  }

-- Null implementation
-- ===================

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
    , storeLatency = 0
    , init = return ()
    , initInProgress = false
    }

-- Simple implementation
-- =====================

-- | Simple implemenation
makeSimpleSIMTRegFile :: Bits t_reg =>
     Maybe t_reg
     -- ^ Optional initialisation file
  -> Module (SIMTRegFile t_reg)
makeSimpleSIMTRegFile m_initVal = do

  -- Register file banks, one per lane
  (banksA, banksB) ::
    ([RAM SIMTRegFileIdx t_reg],
     [RAM SIMTRegFileIdx t_reg]) <-
       unzip <$> replicateM SIMTLanes makeQuadRAM

  -- Initialisation in progress?
  initInProgress <- makeReg false

  -- Initialisation counter
  initIdx <- makeReg 0

  -- Initialisation
  case m_initVal of
    Nothing -> return ()
    Just initVal -> do
      always do
        when initInProgress.val do
          sequence_
            [ bank.store (unpack initIdx.val) initVal
            | bank <- banksB ]
          initIdx <== initIdx.val + 1
          when (initIdx.val .==. ones) do
            initInProgress <== false

  return
    SIMTRegFile {
      loadA = \idx -> sequence_ [bank.load idx | bank <- banksA]
    , loadB = \idx -> sequence_ [bank.load idx | bank <- banksB]
    , outA = fromList [buffer bank.out | bank <- banksA]
    , outB = fromList [buffer bank.out | bank <- banksB]
    , store = \idx vec -> do
        sequence_
          [ when item.valid do bank.store idx item.val
          | (item, bank) <- zip (toList vec) banksA ]
    , storeLatency = 0
    , init = do initInProgress <== true
    , initInProgress = initInProgress.val
    }
