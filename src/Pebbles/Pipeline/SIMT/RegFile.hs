-- SIMT register file implementations
module Pebbles.Pipeline.SIMT.RegFile where

-- SoC configuration
#include <Config.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stack
import Blarney.Option
import Blarney.Stream
import Blarney.PulseWire
import Blarney.QuadPortRAM
import Blarney.Interconnect
import Blarney.TaggedUnion
import Blarney.Vector qualified as V
import Blarney.Vector (Vec, fromList, toList)

-- Pebbles imports
import Pebbles.Util.List
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
    -- ^ Port A's value available one or more cycles after load
  , outB :: Vec SIMTLanes t_reg
    -- ^ Port B's value available one or more cycles after load
  , isScalarA :: Bit 1
    -- ^ Is Port A a scalar (valid when outA is valid)
  , isScalarB :: Bit 1
    -- ^ Is Port B a scalar (valid when outB is valid)
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
    , isScalarA = false
    , isScalarB = false
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
     Int
     -- ^ Desired load latency
  -> Maybe t_reg
     -- ^ Optional initialisation value
  -> Module (SIMTRegFile t_reg)
makeSimpleSIMTRegFile loadLatency m_initVal = do

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
    , outA = fromList [ iterateN (loadLatency-1) buffer bank.out
                      | bank <- banksA ]
    , outB = fromList [ iterateN (loadLatency-1) buffer bank.out
                      | bank <- banksB ]
    , isScalarA = false
    , isScalarB = false
    , store = \idx vec -> do
        sequence_
          [ when item.valid do bank.store idx item.val
          | (item, bank) <- zip (toList vec) banksA ]
    , storeLatency = 0
    , init =
        case m_initVal of
          Nothing -> return ()
          Just _ -> do initInProgress <== true
    , initInProgress = initInProgress.val
    , maxVecRegs = fromInteger (SIMTWarps * 32)
    }

-- Basic scalarising implementation
-- ================================

-- This scalarising register file consists of a scalar register
-- file containing scalar values or pointers to a dynamically
-- growing/shrinking vector scratchpad.

-- | Pointer to vector in scratchpad
type SpadPtr = SIMTRegFileAddr

-- | A scalar register is either a scalar val or a pointer to a vector
-- in the scratchpad
type ScalarReg t_reg =
  TaggedUnion [
    "scalar" ::: t_reg
  , "vector" ::: SpadPtr
  ]

-- | Load latency of this implementation
basicSIMTScalarisingRegFile_loadLatency :: Int = 3

-- | Basic scalarising register file (all or nothing)
makeBasicSIMTScalarisingRegFile :: forall t_reg. (Bits t_reg, Cmp t_reg) =>
     t_reg
     -- ^ Initial register value
  -> Module (SIMTRegFile t_reg)
makeBasicSIMTScalarisingRegFile initVal = do

  -- Scalar register file (4 read ports, 2 write ports)
  (scalarRegFileA, scalarRegFileB) ::
    (RAM SIMTRegFileIdx (ScalarReg t_reg),
     RAM SIMTRegFileIdx (ScalarReg t_reg)) <- makeQuadRAM
  (scalarRegFileC, scalarRegFileD) ::
    (RAM SIMTRegFileIdx (ScalarReg t_reg),
     RAM SIMTRegFileIdx (ScalarReg t_reg)) <- makeQuadRAM

  -- Vector scratchpad (banked)
  (vecSpadA, vecSpadB) ::
    ([RAM SIMTRegFileAddr t_reg],
     [RAM SIMTRegFileAddr t_reg]) <-
       unzip <$> replicateM SIMTLanes makeQuadRAM

  -- Stack of free space in vector scratchpad
  freeSlots :: Stack SIMTRegFileAddr <-
    makeSizedStack (SIMTLogWarps + 5)

  -- Count number of vectors in use
  vecCount :: Reg (Bit (SIMTLogWarps + 6)) <- makeReg 0

  -- Track max vector count
  maxVecCount :: Reg (Bit (SIMTLogWarps + 6)) <- makeReg 0

  -- Initialisation
  -- ==============

  -- Initialisation in progress?
  initInProgress <- makeReg false

  -- Initialisation counter
  initIdx <- makeReg ones

  -- Initialisation
  always do
    when initInProgress.val do
      let initScalarVal = tag #scalar initVal
      let idx = unpack initIdx.val
      scalarRegFileB.store idx initScalarVal
      scalarRegFileD.store idx initScalarVal
      freeSlots.push initIdx.val
      initIdx <== initIdx.val - 1
      when (initIdx.val .==. 0) do
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
      let isVectorA = scalarRegFileA.out `is` #vector
      when isVectorA do
        sequence_
          [ bank.load (untag #vector scalarRegFileA.out)
          | bank <- vecSpadA ]
      let isVectorB = scalarRegFileB.out `is` #vector
      when isVectorB do
        sequence_
          [ bank.load (untag #vector scalarRegFileB.out)
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
  storeScalarEntry2 <- makeReg dontCare

  always do
    -- Stage 1
    when store1.val do
      let newVec :: Vec SIMTLanes (Option t_reg) = fromList
            [ item.valid ? (item, Option (scalarRegFileC.out `is` #scalar)
                                         (untag #scalar scalarRegFileC.out))
            | item <- toList storeVec1.val ]
      -- Is it a scalar write?
      let isScalar = allEqual (toList newVec)
      let anyValid = orList [item.valid | item <- toList storeVec1.val]
      -- Trigger next stage
      storeIsScalar2 <== isScalar
      storeScalarEntry2 <== scalarRegFileC.out
      storeIdx2 <== storeIdx1.val
      storeVec2 <== newVec
      when anyValid do
        store2 <== true

    -- Stage 2
    when store2.val do
      -- Was it a vector before this write?
      let wasVector = storeScalarEntry2.val `is` #vector
      -- Is it a vector after this write?
      let isVector = inv storeIsScalar2.val

      if isVector
        then do
          -- Now a vector. Was it a scalar before?
          when (inv wasVector) do
            -- We need to allocate space for a new vector
            dynamicAssert freeSlots.notEmpty
              "Scalarising reg file: out of free space"
            freeSlots.pop
            vecCount <== vecCount.val + 1
            -- Tell scalar reg file about new vector
            let newScalarRegEntry = tag #vector freeSlots.top
            scalarRegFileA.store storeIdx2.val newScalarRegEntry
            scalarRegFileC.store storeIdx2.val newScalarRegEntry
          -- Write to vector scratchpad
          let spadAddr = wasVector ?
                (untag #vector storeScalarEntry2.val, freeSlots.top)
          sequence_
            [ when item.valid do bank.store spadAddr item.val
            | (bank, item) <- zip vecSpadA (toList storeVec2.val) ]
        else do
          -- Now a scalar. Was it a vector before?
          when wasVector do
            -- We need to reclaim the vector
            dynamicAssert freeSlots.notFull
              "Scalarising reg file: free slot overflow"
            freeSlots.push (untag #vector storeScalarEntry2.val)
            vecCount <== vecCount.val - 1
          -- Write to scalar reg file
          let scalarVal = V.head storeVec2.val
          scalarRegFileA.store storeIdx2.val (tag #scalar scalarVal.val)
          scalarRegFileC.store storeIdx2.val (tag #scalar scalarVal.val)

  return
    SIMTRegFile {
      loadA = \idx -> do
        scalarRegFileA.load idx
        loadWireA <== true
    , loadB = \idx -> do
        scalarRegFileB.load idx
        loadWireB <== true
    , outA =
        let isVector = delay false (scalarRegFileA.out `is` #vector) in
          old $ isVector ?
            ( V.fromList [bank.out | bank <- vecSpadA]
            , V.replicate (old $ untag #scalar scalarRegFileA.out) )
    , outB =
        let isVector = delay false (scalarRegFileB.out `is` #vector) in
          old $ isVector ?
            ( V.fromList [bank.out | bank <- vecSpadB]
            , V.replicate (old $ untag #scalar scalarRegFileB.out) )
    , isScalarA = delay false (scalarRegFileA.out `is` #scalar)
    , isScalarB = delay false (scalarRegFileB.out `is` #scalar)
    , store = \idx vec -> do
        store1 <== true
        storeIdx1 <== idx
        storeVec1 <== vec
        scalarRegFileC.load idx
    , storeLatency = 2
    , init = do
        initInProgress <== true
        freeSlots.clear
    , initInProgress = initInProgress.val
    , maxVecRegs = maxVecCount.val
    }
