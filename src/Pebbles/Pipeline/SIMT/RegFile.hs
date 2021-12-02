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
import Blarney.Vector qualified as V
import Blarney.Vector (Vec, fromList, toList)

-- Pebbles imports
import Pebbles.Pipeline.Interface

-- | Per-warp register file index
type SIMTRegFileIdx = (Bit SIMTLogWarps, RegId)
type SIMTRegFileAddr = Bit (SIMTLogWarps + 5)

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
  , maxVecRegs :: Bit (SIMTLogWarps + 6)
    -- ^ Max number of vector registers used
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
    , maxVecRegs = 0
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
    , maxVecRegs = fromInteger (SIMTWarps * 32)
    }

-- Basic scalarising implementation
-- ================================

-- This scalarising register file consists of: (1) a directory of
-- pointers to a dynamically growing/shrinking vector scratchpad; and
-- (2) a per-warp scalar register file.  The scalar reg file has only
-- two read ports, which is somewhat restrictive.

-- | Directory entry
data DirEntry =
  DirEntry {
    isVector :: Bit 1
    -- ^ Is reg stored in scalar reg file or vector scratchpad?
  , spadPtr :: SIMTRegFileAddr
    -- ^ If scratchpad, at what location?
  }
  deriving (Generic, Bits)

-- | Basic scalarising register file (all or nothing)
makeBasicSIMTScalarisingRegFile :: (Bits t_reg, Cmp t_reg) =>
     Maybe t_reg
     -- ^ Optional initialisation file
  -> Module (SIMTRegFile t_reg)
makeBasicSIMTScalarisingRegFile m_initVal = do

  -- Directory (4 read ports, 2 write ports)
  (dirA, dirB) :: (RAM SIMTRegFileIdx DirEntry,
                   RAM SIMTRegFileIdx DirEntry) <- makeQuadRAM
  (dirC, dirD) :: (RAM SIMTRegFileIdx DirEntry,
                   RAM SIMTRegFileIdx DirEntry) <- makeQuadRAM

  -- Scalar register file
  (scalarRegFileA, scalarRegFileB) ::
    (RAM SIMTRegFileIdx t_reg, RAM SIMTRegFileIdx t_reg) <- makeQuadRAM

  -- Vector scratchpad (banked)
  (vecSpadA, vecSpadB) ::
    ([RAM SIMTRegFileAddr (Option t_reg)],
     [RAM SIMTRegFileAddr (Option t_reg)]) <-
       unzip <$> replicateM SIMTLanes makeQuadRAM

  -- Queue of free space in vector scratchpad
  freeSlots :: Queue SIMTRegFileAddr <-
    makeSizedQueue (SIMTLogWarps + 5)

  -- Count number of vectors in use
  vecCount :: Reg (Bit (SIMTLogWarps + 6)) <- makeReg 0

  -- Track max vector count
  maxVecCount :: Reg (Bit (SIMTLogWarps + 6)) <- makeReg 0

  -- Initialisation
  -- ==============

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
          let initDirEntry =
                DirEntry {
                  isVector = false
                , spadPtr = 0
                }
          let idx = unpack initIdx.val
          dirB.store idx initDirEntry
          dirD.store idx initDirEntry
          scalarRegFileB.store idx initVal
          freeSlots.enq initIdx.val
          initIdx <== initIdx.val + 1
          when (initIdx.val .==. ones) do
            vecCount <== 0
            maxVecCount <== 0
            initInProgress <== false

  -- Track max vector count
  always do
    when (inv initInProgress.val) do
      maxVecCount <== vecCount.val .>. maxVecCount.val ?
                        (vecCount.val, maxVecCount.val)

  -- Load path
  -- =========

  -- Load wires
  loadWireA <- makeWire false
  loadWireB <- makeWire false

  always do
    let goLoad = delay false (loadWireA.val .||. loadWireB.val)
    when goLoad do
      -- If vector, issue load to vector scratchpad
      when dirA.out.isVector do
        sequence_
          [ bank.load dirA.out.spadPtr
          | bank <- vecSpadA ]
      when dirB.out.isVector do
        sequence_
          [ bank.load dirA.out.spadPtr
          | bank <- vecSpadB ]

  -- Store path
  -- ==========

  -- Stage 1 pipeline registers
  store1 <- makeDReg false
  storeIdx1 <- makeReg dontCare
  storeVec1 <- makeReg dontCare

  -- Stage 2 pipeline registers
  store2 <- makeDReg false
  storeIdx2 <- makeReg dontCare
  storeVec2 <- makeReg dontCare
  storeIsScalar2 <- makeReg dontCare

  always do
    -- Stage 1
    when store1.val do
      -- Lookup directory
      dirC.load storeIdx1.val
      -- Is it a scalar write
      let headItem = V.head storeVec1.val
      let isScalar = andList
            [ item.valid .&&. item.val .==. headItem.val
            | item <- toList storeVec1.val ]
      -- Trigger next stage
      storeIsScalar2 <== isScalar
      storeIdx2 <== storeIdx1.val
      storeVec2 <== storeVec1.val
      when (orList [item.valid | item <- toList storeVec1.val]) do
        store2 <== true

    -- Stage 2
    when store2.val do
      -- Was it a vector before this write?
      let wasVector = dirC.out.isVector
      -- Is it a vector after this write?
      let isVector = inv storeIsScalar2.val

      if isVector
        then do
          -- Now a vector. Was it a scalar before? If so, update directory
          when (inv wasVector) do
            -- We need to allocate space for a new vector
            dynamicAssert freeSlots.canDeq
              "Scalarising reg file: out of free space"
            freeSlots.deq
            vecCount <== vecCount.val + 1
            -- Tell directory about new vector
            let newDirEntry =
                  DirEntry {
                    isVector = true
                  , spadPtr = freeSlots.first
                  }
            dirA.store storeIdx2.val newDirEntry
            dirC.store storeIdx2.val newDirEntry
          -- Write to vector scratchpad
          let spadAddr = wasVector ? (dirC.out.spadPtr, freeSlots.first)
          sequence_
            [ when (item.valid .||. inv wasVector) do
                bank.store spadAddr $
                  Option {
                    valid = wasVector ? (true, item.valid)
                  , val = item.val
                  }
            | (bank, item) <- zip vecSpadA (toList storeVec2.val) ]
        else do
          -- Now a scalar. Was it a vector before? If so, update directory
          when wasVector do
            -- We need to reclaim the vector
            dynamicAssert freeSlots.notFull
              "Scalarising reg file: free slot overflow"
            freeSlots.enq dirC.out.spadPtr
            vecCount <== vecCount.val - 1
            -- Tell directory about new scalar
            let newDirEntry =
                  DirEntry {
                    isVector = false
                  , spadPtr = dontCare
                  }
            dirA.store storeIdx2.val newDirEntry
            dirC.store storeIdx2.val newDirEntry
          -- Write to scalar reg file
          let scalarVal = V.head storeVec2.val
          when scalarVal.valid do
            scalarRegFileA.store storeIdx2.val scalarVal.val

  return
    SIMTRegFile {
      loadA = \idx -> do
        dirA.load idx
        scalarRegFileA.load idx
        loadWireA <== true
    , loadB = \idx -> do
        dirB.load idx
        scalarRegFileB.load idx
        loadWireB <== true
    , outA = delay false dirA.out.isVector ?
               ( V.fromList [ bank.out.valid ?
                                (bank.out.val, old scalarRegFileA.out)
                            | bank <- vecSpadA ]
               , V.replicate (old scalarRegFileA.out) )
    , outB = delay false dirB.out.isVector ?
               ( V.fromList [ bank.out.valid ?
                                (bank.out.val, old scalarRegFileB.out)
                            | bank <- vecSpadB ]
               , V.replicate (old scalarRegFileB.out) )
    , store = \idx vec -> do
        store1 <== true
        storeIdx1 <== idx
        storeVec1 <== vec
    , storeLatency = 2
    , init = do
        initInProgress <== true
        freeSlots.clear
    , initInProgress = initInProgress.val
    , maxVecRegs = maxVecCount.val
    }
