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

-- Info about a register
data RegInfo =
  RegInfo {
    isScalar :: Bit 1
    -- ^ Is it a scalar?
  , isUniform :: Bit 1
    -- ^ Is it uniform (i.e. not affine)?
  }
  deriving (Generic, Bits)

-- | Vector register file
data SIMTRegFile regWidth =
  SIMTRegFile {
    loadA :: SIMTRegFileIdx -> Action ()
    -- ^ Issue load for given register on port A
  , loadB :: SIMTRegFileIdx -> Action ()
    -- ^ Issue load for given register on port B
  , outA :: Vec SIMTLanes (Bit regWidth)
    -- ^ Port A's value available one or more cycles after load
  , outB :: Vec SIMTLanes (Bit regWidth)
    -- ^ Port B's value available one or more cycles after load
  , infoA :: RegInfo
    -- ^ Port A reg info (valid when outA is valid)?
  , infoB :: RegInfo
    -- ^ Port B reg info (valid when outB is valid)?
  , store :: SIMTRegFileIdx
          -> Vec SIMTLanes (Option (Bit regWidth))
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
makeNullSIMTRegFile :: KnownNat regWidth => Module (SIMTRegFile regWidth)
makeNullSIMTRegFile = do
  return
    SIMTRegFile {
      loadA = \_ -> return ()
    , loadB = \_ -> return ()
    , outA = dontCare
    , outB = dontCare
    , infoA = RegInfo false false
    , infoB = RegInfo false false
    , store = \_ _ -> return ()
    , storeLatency = 0
    , init = return ()
    , initInProgress = false
    , maxVecRegs = 0
    }

-- Plain register file implementation
-- ==================================

-- | Plain implemenation
makeSIMTRegFile :: KnownNat regWidth =>
     Int
     -- ^ Desired load latency
  -> Maybe (Bit regWidth)
     -- ^ Optional initialisation value
  -> Module (SIMTRegFile regWidth)
makeSIMTRegFile loadLatency m_initVal = do

  -- Register file banks, one per lane
  (banksA, banksB) ::
    ([RAM SIMTRegFileIdx (Bit regWidth)],
     [RAM SIMTRegFileIdx (Bit regWidth)]) <-
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
    , infoA = RegInfo false false
    , infoB = RegInfo false false
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
type ScalarReg regWidth =
  TaggedUnion [
    "scalar" ::: ScalarVal regWidth
  , "vector" ::: SpadPtr
  ]

-- | This structure represents a unform or affine vector
data ScalarVal n =
  ScalarVal {
    val :: Bit n
    -- ^ Value
  , stride :: Bit SIMTAffineScalarisationBits
    -- ^ Stride between values of an affine vector;
    -- if stride is 0 then the vector is uniform
  }
  deriving (Generic, Bits)

-- | Load latency of this implementation
simtScalarisingRegFile_loadLatency :: Int = 3

-- | Scalarising implementation
makeSIMTScalarisingRegFile :: forall regWidth. KnownNat regWidth =>
     Bool
     -- ^ Use affine scalarisation?
  -> Bit regWidth
     -- ^ Initial register value
  -> Module (SIMTRegFile regWidth)
makeSIMTScalarisingRegFile useAffine initVal = do

  -- Scalar register file (4 read ports, 2 write ports)
  (scalarRegFileA, scalarRegFileB) ::
    (RAM SIMTRegFileIdx (ScalarReg regWidth),
     RAM SIMTRegFileIdx (ScalarReg regWidth)) <- makeQuadRAM
  (scalarRegFileC, scalarRegFileD) ::
    (RAM SIMTRegFileIdx (ScalarReg regWidth),
     RAM SIMTRegFileIdx (ScalarReg regWidth)) <- makeQuadRAM

  -- Vector scratchpad (banked)
  (vecSpadA, vecSpadB) ::
    ([RAM SIMTRegFileAddr (Bit regWidth)],
     [RAM SIMTRegFileAddr (Bit regWidth)]) <-
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
      let initScalarReg = tag #scalar
            ScalarVal {
              val = initVal
            , stride = 0
            }
      let idx = unpack initIdx.val
      scalarRegFileB.store idx initScalarReg
      scalarRegFileD.store idx initScalarReg
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

  -- Affine offsets
  affineOffsetsA ::
    [Reg (Bit (SIMTAffineScalarisationBits+SIMTLogLanes))] <-
      replicateM SIMTLanes (makeReg 0)
  affineOffsetsB ::
    [Reg (Bit (SIMTAffineScalarisationBits+SIMTLogLanes))] <-
      replicateM SIMTLanes (makeReg 0)

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
      -- Compute affine offsets
      when useAffine do
        let scalarA = untag #scalar scalarRegFileA.out
        let scalarB = untag #scalar scalarRegFileB.out
        zipWithM_ (<==) affineOffsetsA
          [ zeroExtend scalarA.stride * fromInteger i
          | i <- [0..SIMTLanes-1] ]
        zipWithM_ (<==) affineOffsetsB
          [ zeroExtend scalarB.stride * fromInteger i
          | i <- [0..SIMTLanes-1] ]

  -- Store path
  -- ==========

  -- Stage 1 pipeline registers
  store1 <- makeDReg false
  storeIdx1 <- makeReg dontCare
  storeVec1 <- makeReg dontCare
  storeStride1 :: Reg (Bit SIMTAffineScalarisationBits) <- makeReg 0

  -- Stage 2 pipeline registers
  store2 <- makeDReg false
  storeIdx2 <- makeReg dontCare
  storeVec2 <- makeReg dontCare
  storeIsScalar2 <- makeReg dontCare
  storeScalarEntry2 <- makeReg dontCare
  storeStride2 :: Reg (Bit SIMTAffineScalarisationBits) <- makeReg 0
  storeOffsets2 ::
    [Reg (Bit (SIMTAffineScalarisationBits+SIMTLogLanes))] <-
      replicateM SIMTLanes (makeReg 0)

  always do
    -- Stage 1
    when store1.val do
      -- Scalar reg before write
      let scalarReg = untag #scalar scalarRegFileC.out
      -- Is it a uniform vector?
      let isUniform = scalarRegFileC.out `is` #scalar .&&.
                        (if useAffine
                           then scalarReg.stride .==. 0
                           else true)
      -- Validity of writes
      let anyValid = orList [item.valid | item <- toList storeVec1.val]
      let allValid = andList [item.valid | item <- toList storeVec1.val]
      -- Compute new vector to write
      let writeVals :: Vec SIMTLanes (Bit regWidth) = fromList
            [ item.valid ? (item.val, scalarReg.val)
            | item <- toList storeVec1.val ]
      -- Comparison function for detecting uniform/affine vectors
      let equal x y =
            if useAffine
              then x + zeroExtendCast storeStride1.val .==. y
              else x .==. y
      -- Is it a scalar write?
      let isScalar = (allValid .||. isUniform) .&&.
            andList (zipWith equal (toList writeVals)
                                   (drop 1 $ toList writeVals))
      -- Trigger next stage
      storeIsScalar2 <== isScalar
      storeScalarEntry2 <== scalarRegFileC.out
      storeIdx2 <== storeIdx1.val
      storeVec2 <== V.zipWith (\item writeVal -> Option item.valid writeVal)
                      storeVec1.val writeVals
      storeStride2 <== storeStride1.val
      when anyValid do
        store2 <== true
      -- Expand affine vector
      when useAffine do
        zipWithM_ (<==) storeOffsets2
          [ item.valid ? (0, zeroExtend scalarReg.stride * fromInteger i)
          | (item, i) <- zip (toList storeVec1.val) [0..SIMTLanes-1] ]
 
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
          let isAffine = storeScalarEntry2.val `is` #scalar
          sequence_
            [ when (item.valid .||. inv wasVector) do
                if useAffine
                  then bank.store spadAddr
                         (item.val + zeroExtendCast offset.val)
                  else bank.store spadAddr item.val
            | (bank, item, offset) <-
                zip3 vecSpadA (toList storeVec2.val) storeOffsets2 ]
        else do
          -- Now a scalar. Was it a vector before?
          when wasVector do
            -- We need to reclaim the vector
            dynamicAssert freeSlots.notFull
              "Scalarising reg file: free slot overflow"
            freeSlots.push (untag #vector storeScalarEntry2.val)
            vecCount <== vecCount.val - 1
          -- Write to scalar reg file
          let scalarVal =
                ScalarVal {
                  val = (V.head storeVec2.val).val
                , stride = storeStride2.val
                }
          scalarRegFileA.store storeIdx2.val (tag #scalar scalarVal)
          scalarRegFileC.store storeIdx2.val (tag #scalar scalarVal)

  -- Expand scalar register to vector
  let expandScalar scalarReg offsets = 
        if useAffine
          then fromList [ scalarReg.val + zeroExtendCast o.val
                        | o <- offsets ]
          else V.replicate scalarReg.val

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
            , expandScalar (old $ untag #scalar scalarRegFileA.out)
                           affineOffsetsA )
    , outB =
        let isVector = delay false (scalarRegFileB.out `is` #vector) in
          old $ isVector ?
            ( V.fromList [bank.out | bank <- vecSpadB]
            , expandScalar (old $ untag #scalar scalarRegFileB.out)
                           affineOffsetsB )
    , infoA = delay (RegInfo false false)
        RegInfo {
          isScalar = delay false (scalarRegFileA.out `is` #scalar)
        , isUniform = delay false (scalarRegFileA.out `is` #scalar .&&.
                        (untag #scalar scalarRegFileA.out).stride .==. 0)
        }
    , infoB = delay (RegInfo false false)
        RegInfo {
          isScalar = delay false (scalarRegFileB.out `is` #scalar)
        , isUniform = delay false (scalarRegFileB.out `is` #scalar .&&.
                        (untag #scalar scalarRegFileB.out).stride .==. 0)
        }
    , store = \idx vec -> do
        store1 <== true
        storeIdx1 <== idx
        storeVec1 <== vec
        scalarRegFileC.load idx
        -- Determine stride for affine scalarisation
        when useAffine do
          let v0 = vec ! (0::Int)
          let v1 = vec ! (1::Int)
          let diff = v1.val - v0.val
          let mask = fromInteger (2^SIMTAffineScalarisationBits - 1)
          storeStride1 <==
            if v0.valid .&&. v1.valid .&&. (diff .&. inv mask .==. 0)
              then truncateCast diff
              else 0
    , storeLatency = 2
    , init = do
        initInProgress <== true
        freeSlots.clear
    , initInProgress = initInProgress.val
    , maxVecRegs = maxVecCount.val
    }
