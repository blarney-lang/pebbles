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
import Pebbles.Util.SlotManager
import Pebbles.Pipeline.Interface

-- CHERI imports
import CHERI.CapLib

-- Haskell imports
import Data.Proxy

-- | Per-warp register file index
type SIMTRegFileIdx = (Bit SIMTLogWarps, RegId)
type SIMTRegFileAddr = Bit (SIMTLogWarps + 5)

-- | This structure represents a unform or affine vector
data ScalarVal n =
  ScalarVal {
    val :: Bit n
    -- ^ Value
  , stride :: Bit SIMTAffineScalarisationBits
    -- ^ Stride between values of an affine vector;
    -- if stride is 0 then the vector is uniform
  }
  deriving (Generic, Interface, Bits)

-- | Scalarised instruction operand
data ScalarisedOperand =
  ScalarisedOperand {
    scalarisedVal :: Option (ScalarVal 32)
  , scalarisedCapVal :: Option (ScalarVal CapMemMetaWidth)
  } deriving (Generic, Interface, Bits)

-- | Vector register file
data SIMTRegFile t_logSize regWidth =
  SIMTRegFile {
    loadA :: SIMTRegFileIdx -> Action ()
    -- ^ Issue load for given register on port A
  , loadB :: SIMTRegFileIdx -> Action ()
    -- ^ Issue load for given register on port B
  , loadScalarC :: SIMTRegFileIdx -> Action ()
    -- ^ Issue load for given scalar register on port C
  , loadScalarD :: SIMTRegFileIdx -> Action ()
    -- ^ Issue load for given scalar register on port D
  , outA :: Vec SIMTLanes (Bit regWidth)
    -- ^ Port A's value available one or more cycles after load
  , outB :: Vec SIMTLanes (Bit regWidth)
    -- ^ Port B's value available one or more cycles after load
  , evictedA :: Bit 1
    -- ^ Port A vector register eviction status (valid when 'outA' is valid)
  , evictedB :: Bit 1
    -- ^ Port B vector register eviction status (valid when 'outB' is valid)
  , loadEvictedStatus :: SIMTRegFileIdx -> Action ()
    -- ^ Issue load of evicted status of given register
  , evictedStatus :: Bit 1
    -- ^ Vector register eviction status
  , scalarA :: Option (ScalarVal regWidth)
    -- ^ Port A scalar (valid when 'outA' is valid)
  , scalarB :: Option (ScalarVal regWidth)
    -- ^ Port B scalar (valid when 'outB' is valid)
  , scalarC :: Option (ScalarVal regWidth)
    -- ^ Port C scalar (valid one cycle after 'scalarLoadC')
  , scalarD :: Option (ScalarVal regWidth)
    -- ^ Port D scalar (valid one cycle after 'scalarLoadD')
  , store :: SIMTRegFileIdx
          -> Vec SIMTLanes (Option (Bit regWidth))
          -> Action ()
    -- ^ Write register
  , canStore :: SIMTRegFileIdx -> Bit 1
    -- ^ Backpressure on stores to a given register
  , evict :: SIMTRegFileIdx -> Action ()
    -- ^ Mark a register as a "vector evicted to DRAM"
  , storeScalar :: SIMTRegFileIdx
                -> ScalarVal regWidth
                -> Action ()
    -- ^ Write scalar register (can be called in parallel to 'store').
    -- Store occurs on cycle after method is called.
  , storeLatency :: Int
    -- ^ Number of cycles after 'store' before write is performed
  , init :: Action ()
    -- ^ Trigger initialisation of register file
  , initInProgress :: Bit 1
    -- ^ Don't trigger intialisation while it is already in progress
  , numVecRegsUnused :: Bit (SIMTLogWarps + 7)
    -- ^ Current number of vector registers unused
  , numVecRegs :: Bit (SIMTLogWarps + 7)
    -- ^ Current number of vector registers unused
  , maxVecRegs :: Bit (SIMTLogWarps + 7)
    -- ^ Max number of vector registers used
  , totalVecRegs :: Bit 32
    -- ^ Total vector registers, sampled at parameterised rate
  , getVecMasks :: Vec SIMTWarps (Bit 32)
    -- ^ Which registers for each warp are vectors?
  , sharedVecSpad :: SharedVecSpad t_logSize regWidth
    -- ^ Use shared vector scratchpad
  , stall :: Bit 1
    -- ^ Stall load pipeline
  }

-- | Vector scratchpad can be shared between register files
data SharedVecSpad t_logSize regWidth =
  SharedVecSpad {
    vecSpadA :: [RAM (Bit t_logSize) (Bit regWidth)]
  , vecSpadB :: [RAM (Bit t_logSize) (Bit regWidth)]
  , freeSlots :: SlotManager (Bit t_logSize)
  }

-- Null implementation
-- ===================

-- | Null implementation
makeNullSIMTRegFile ::
     (KnownNat t_logSize, KnownNat regWidth)
  => Module (SIMTRegFile t_logSize regWidth)
makeNullSIMTRegFile = do
  return
    SIMTRegFile {
      loadA = \_ -> return ()
    , loadB = \_ -> return ()
    , loadScalarC = \_ -> return ()
    , loadScalarD = \_ -> return ()
    , outA = dontCare
    , outB = dontCare
    , evictedA = false
    , evictedB = false
    , loadEvictedStatus = \_ -> return ()
    , evictedStatus = false
    , scalarA = none
    , scalarB = none
    , scalarC = none
    , scalarD = none
    , store = \_ _ -> return ()
    , canStore = \_ -> true
    , evict = \_ -> return ()
    , storeScalar = \_ _ -> return ()
    , storeLatency = 0
    , init = return ()
    , initInProgress = false
    , maxVecRegs = fromInteger (SIMTWarps * 32)
    , totalVecRegs = 0
    , numVecRegsUnused = 0
    , numVecRegs = fromInteger (SIMTWarps * 32)
    , getVecMasks = V.replicate ones
    , sharedVecSpad = error "Null SIMT regfile does not produce vec spad"
    , stall = false
    }

-- Plain register file implementation
-- ==================================

-- | SIMTRegFile config options
data SIMTRegFileConfig regWidth =
  SIMTRegFileConfig {
    loadLatency :: Int
    -- ^ Desired load latency of register file
  , regInitVal :: Maybe (Bit regWidth)
    -- ^ Optional initialisation value for registers
  }

-- | Plain implemenation
makeSIMTRegFile :: (KnownNat t_logSize, KnownNat regWidth) =>
     SIMTRegFileConfig regWidth
     -- ^ Config options
  -> Module (SIMTRegFile t_logSize regWidth)
makeSIMTRegFile opts = do

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
  case opts.regInitVal of
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
    , loadScalarC = error "makeSIMTRegFile: loadScalarC unavailable"
    , loadScalarD = error "makeSIMTRegFile: loadScalarD unavailable"
    , outA = fromList [ iterateN (opts.loadLatency-1) buffer bank.out
                      | bank <- banksA ]
    , outB = fromList [ iterateN (opts.loadLatency-1) buffer bank.out
                      | bank <- banksB ]
    , evictedA = false
    , evictedB = false
    , loadEvictedStatus = \_ -> return ()
    , evictedStatus = false
    , scalarA = none
    , scalarB = none
    , scalarC = none
    , scalarD = none
    , store = \idx vec -> do
        sequence_
          [ when item.valid do bank.store idx item.val
          | (item, bank) <- zip (toList vec) banksA ]
    , canStore = \_ -> true
    , evict = \_ -> return ()
    , storeScalar = error "makeSIMTRegFile: storeScalar unavailable"
    , storeLatency = 0
    , init =
        case opts.regInitVal of
          Nothing -> return ()
          Just _ -> do initInProgress <== true
    , initInProgress = initInProgress.val
    , maxVecRegs = fromInteger (SIMTWarps * 32)
    , totalVecRegs = 0
    , numVecRegs = fromInteger (SIMTWarps * 32)
    , numVecRegsUnused = 0
    , getVecMasks = V.replicate ones
    , sharedVecSpad = error "SIMT regfile does not produce vec spad"
    , stall = false
    }

-- Basic scalarising implementation
-- ================================

-- This scalarising register file consists of a scalar register
-- file containing scalar values or pointers to a dynamically
-- growing/shrinking vector scratchpad.

-- | Pointer to vector in scratchpad
type SpadPtr = SIMTRegFileAddr

-- | A scalar register is either a scalar val or a pointer to a vector
-- in the scratchpad; it can also be a vector that has been evicted to
-- DRAM due to scratchpad overflow.
type ScalarReg regWidth =
  TaggedUnion [
    "scalar" ::: ScalarVal regWidth
  , "vector" ::: SpadPtr
  , "evicted" ::: ()
  ]

-- | Load latency of this implementation
simtScalarisingRegFile_loadLatency :: Int = 3

-- | SIMTScalarisingRegFile config options
data SIMTScalarisingRegFileConfig t_logSize regWidth =
  SIMTScalarisingRegFileConfig {
    useAffine :: Bool
    -- ^ Use affine scalarisation?
  , useScalarUnit :: Bool
    -- ^ Provide extra ports for scalar unit accesses?
  , regInitVal :: Bit regWidth
    -- ^ Initial register value
  , size :: Int
    -- ^ Size of vector scratchpad (in vectors)
  , useDynRegSpill :: Bool
    -- ^ Enable features to support dynamic register spilling
    -- e.g. track which registers are vectors and which are evicted
  , useSharedVecSpad :: Maybe (SharedVecSpad t_logSize regWidth)
    -- ^ Use shared vector scratchpad
  }

-- | Scalarising implementation
makeSIMTScalarisingRegFile :: forall t_logSize regWidth.
     (KnownNat t_logSize, KnownNat regWidth)
  => SIMTScalarisingRegFileConfig t_logSize regWidth
     -- ^ Config options
  -> Module (SIMTRegFile t_logSize regWidth)
makeSIMTScalarisingRegFile opts = do

  -- Scalar register file (6 read ports, 2 write ports)
  (scalarRegFileA, scalarRegFileB) ::
    (RAM SIMTRegFileIdx (ScalarReg regWidth),
     RAM SIMTRegFileIdx (ScalarReg regWidth)) <- makeQuadRAM
  (scalarRegFileC, scalarRegFileD) ::
    (RAM SIMTRegFileIdx (ScalarReg regWidth),
     RAM SIMTRegFileIdx (ScalarReg regWidth)) <-
       if opts.useScalarUnit
         then makeQuadRAM
         else return (nullRAM, nullRAM)
  (scalarRegFileE, scalarRegFileF) ::
    (RAM SIMTRegFileIdx (ScalarReg regWidth),
     RAM SIMTRegFileIdx (ScalarReg regWidth)) <- makeQuadRAM

  -- Vector scratchpad (banked)
  (vecSpadA, vecSpadB) ::
    ([RAM (Bit t_logSize) (Bit regWidth)],
     [RAM (Bit t_logSize) (Bit regWidth)]) <-
       case opts.useSharedVecSpad of
         Nothing -> unzip <$> replicateM SIMTLanes makeQuadRAM
         Just spad -> return (spad.vecSpadA, spad.vecSpadB)

  -- Stack of free space in vector scratchpad
  freeSlots :: SlotManager (Bit t_logSize) <-
    case opts.useSharedVecSpad of
      Nothing -> makeSlotManager (valueOf @t_logSize)
      Just spad -> return spad.freeSlots

  -- For each warp, maintain a 32-bit mask indicating which
  -- registers are (non-evicted) vectors
  vecMasks :: Vec SIMTWarps (Vec 32 (Reg (Bit 1))) <-
    V.replicateM (V.replicateM (makeReg dontCare))

  -- Eviction status of each register
  (evictStatus, evictStatusB) :: (RAM SIMTRegFileIdx (Bit 1),
                                  RAM SIMTRegFileIdx (Bit 1)) <-
    if opts.useDynRegSpill then makeQuadRAM
                           else return (nullRAM, nullRAM)

  -- Count number of vectors in use
  vecCount :: Reg (Bit (SIMTLogWarps + 7)) <- makeReg 0

  -- Pulse wires to incr/decr vecCount
  vecCountIncr <- makePulseWire
  vecCountDecr1 <- makePulseWire
  vecCountDecr2 <- makePulseWire

  -- Track max vector count
  maxVecCount :: Reg (Bit (SIMTLogWarps + 7)) <- makeReg 0

  -- For determining averge vector scratchpad occupancy
  totalVecCount :: Reg (Bit 32) <- makeReg 0

  let enSharedVecSpad = 
        case opts.useSharedVecSpad of
          Nothing -> False
          Just spad -> True

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
              val = opts.regInitVal
            , stride = 0
            }
      let idx = unpack initIdx.val
      scalarRegFileB.store idx initScalarReg
      scalarRegFileD.store idx initScalarReg
      scalarRegFileF.store idx initScalarReg
      evictStatus.store idx false
      when (not enSharedVecSpad) do
        when (initIdx.val .<=. fromIntegral (opts.size - 1)) do
          freeSlots.push1 (truncateCast initIdx.val)
      when opts.useDynRegSpill do
        sequence_ [ sequence_ [ b <== false | b <- toList mask ]
                  | mask <- toList vecMasks ]
      initIdx <== initIdx.val - 1
      when (initIdx.val .==. 0) do
        vecCount <== 0
        maxVecCount <== 0
        totalVecCount <== 0
        initInProgress <== false

  -- Track max vector count
  always do
    when (inv initInProgress.val) do
      maxVecCount <== vecCount.val .>. maxVecCount.val ?
                        (vecCount.val, maxVecCount.val)

  -- Vector count
  -- ============

  sampleCount :: Reg (Bit 32) <- makeReg 0

  always do
    when (orList [vecCountIncr.val, vecCountDecr1.val
                                  , vecCountDecr2.val]) do
      vecCount <== vecCount.val + zeroExtend vecCountIncr.val
                                - zeroExtend vecCountDecr1.val
                                - zeroExtend vecCountDecr2.val

    if sampleCount.val .==. SIMTTotalVecCountSampleRate
      then do
        sampleCount <== 0
        totalVecCount <== totalVecCount.val + zeroExtend vecCount.val
      else sampleCount <== sampleCount.val + 1

  -- Load path
  -- =========

  -- Load wires
  loadWireA <- makeWire false
  loadWireB <- makeWire false

  -- Stall load pipeline (to handle shared vector scratchpad)
  loadStallWire <- makePulseWire

  -- Affine offsets
  affineOffsetsA ::
    [Reg (Bit (SIMTAffineScalarisationBits+SIMTLogLanes))] <-
      replicateM SIMTLanes (makeReg 0)
  affineOffsetsB ::
    [Reg (Bit (SIMTAffineScalarisationBits+SIMTLogLanes))] <-
      replicateM SIMTLanes (makeReg 0)

  -- (TODO: don't issue load if not load wire not active?)
  always do
    let goLoad = delay false (loadWireA.val .||. loadWireB.val)
    let isVectorA = scalarRegFileA.out `is` #vector
    let isVectorB = scalarRegFileB.out `is` #vector
    let idxA = truncateCast (untag #vector scalarRegFileA.out)
    let idxB = truncateCast (untag #vector scalarRegFileB.out)
    let stall = if enSharedVecSpad
                  then goLoad .&&. (isVectorA .||. isVectorB)
                  else false
    when stall do 
      loadStallWire.pulse

    if enSharedVecSpad
      then do
        when (delay false stall) do
          sequence_ [ bank.load (old idxA) | bank <- vecSpadA ]
          sequence_ [ bank.load (old idxB) | bank <- vecSpadB ]
      else do
        when goLoad do
          sequence_ [ bank.load idxA | bank <- vecSpadA ]
          sequence_ [ bank.load idxB | bank <- vecSpadB ]

    -- Compute affine offsets
    when goLoad do
      when opts.useAffine do
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
  storeEvict1 <- makeDReg false
  storeStride1 :: Reg (Bit SIMTAffineScalarisationBits) <- makeReg 0

  -- Stage 2 pipeline registers
  store2 <- makeDReg false
  storeIdx2 <- makeReg dontCare
  storeVec2 <- makeReg dontCare
  storeEvict2 <- makeDReg false
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
      let scalarReg = untag #scalar scalarRegFileE.out
      -- Is it a uniform vector?
      let isUniform = scalarRegFileE.out `is` #scalar .&&.
                        (if opts.useAffine
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
            if opts.useAffine
              then x + zeroExtendCast storeStride1.val .==. y
              else x .==. y
      -- Is it a scalar write?
      -- If so, all bits in this list will be true. For timing
      -- reasons, we "and" them together in the next cycle, not here
      let isScalarConds = (allValid .||. isUniform) :
            zipWith equal (toList writeVals)
                          (drop 1 $ toList writeVals)
      -- Trigger next stage
      storeIsScalar2 <== (fromBitList isScalarConds :: Bit SIMTLanes)
      storeScalarEntry2 <== scalarRegFileE.out
      storeIdx2 <== storeIdx1.val
      storeVec2 <== V.zipWith (\item writeVal -> Option item.valid writeVal)
                      storeVec1.val writeVals
      storeStride2 <== storeStride1.val
      storeEvict2 <== storeEvict1.val
      when (anyValid .||. storeEvict1.val) do
        store2 <== true
      -- Expand affine vector
      when opts.useAffine do
        zipWithM_ (<==) storeOffsets2
          [ item.valid ? (0, zeroExtend scalarReg.stride * fromInteger i)
          | (item, i) <- zip (toList storeVec1.val) [0..SIMTLanes-1] ]
 
    -- Stage 2
    when store2.val do
      -- Was it a vector before this write?
      let wasVector = storeScalarEntry2.val `is` #vector
      -- Was it an evicted vector before this write?
      let wasEvicted = storeScalarEntry2.val `is` #evicted
      -- Was it a scalar before this write?
      let wasScalar = storeScalarEntry2.val `is` #scalar
      -- Is it a vector after this write?
      let isVector = if SIMTRegFilePreventScalarDetection == 1
                       then true
                       else inv (storeIsScalar2.val .==. ones)
      -- Next free slot
      let slot = freeSlots.top1

      if isVector .&&. inv storeEvict2.val
        then do
          -- Now a vector. Was it a scalar (or evicted vector) before?
          when (wasScalar .||. wasEvicted) do
            -- We need to allocate space for a new vector
            dynamicAssert freeSlots.notEmpty
              "Scalarising reg file: out of free space"
            freeSlots.pop1
            vecCountIncr.pulse
            -- Tell scalar reg file about new vector
            let newScalarRegEntry = tag #vector (zeroExtendCast slot)
            scalarRegFileA.store storeIdx2.val newScalarRegEntry
            scalarRegFileC.store storeIdx2.val newScalarRegEntry
            scalarRegFileE.store storeIdx2.val newScalarRegEntry
            evictStatus.store storeIdx2.val false
          -- Write to vector scratchpad
          let spadAddr = wasVector ?
                ( truncateCast (untag #vector storeScalarEntry2.val)
                , slot )
          let isAffine = storeScalarEntry2.val `is` #scalar
          sequence_
            [ when (item.valid .||. inv wasVector) do
                if opts.useAffine
                  then bank.store spadAddr
                         (item.val + zeroExtendCast offset.val)
                  else bank.store spadAddr item.val
            | (bank, item, offset) <-
                zip3 vecSpadA (toList storeVec2.val) storeOffsets2 ]
        else do
          -- Now a scalar (or evicted vector). Was it a vector before?
          when wasVector do
            -- We need to reclaim the vector
            dynamicAssert freeSlots.notFull
              "Scalarising reg file: free slot overflow"
            freeSlots.push1 (truncateCast
              (untag #vector storeScalarEntry2.val))
            vecCountDecr1.pulse
          -- Write to scalar reg file
          let scalarVal =
                ScalarVal {
                  val = (V.head storeVec2.val).val
                , stride = storeStride2.val
                }
          let writeVal = storeEvict2.val ?
                (tag #evicted (), tag #scalar scalarVal)
          
          scalarRegFileA.store storeIdx2.val writeVal
          scalarRegFileC.store storeIdx2.val writeVal
          scalarRegFileE.store storeIdx2.val writeVal
          evictStatus.store storeIdx2.val storeEvict2.val

      -- Track vectors
      when opts.useDynRegSpill do
        let (warpId, regId) = storeIdx2.val
        let isVec = isVector .&&. inv storeEvict2.val
        ((vecMasks ! warpId) ! regId) <== isVec

  -- Scalar store path
  -- =================

  -- Pipeline registers
  storeScalarGo <- makeDReg false
  storeScalarIdx <- makeReg dontCare
  storeScalarVal <- makeReg dontCare

  always do
    when opts.useScalarUnit do
      when storeScalarGo.val do
        -- Update scalar reg file
        let s = tag #scalar storeScalarVal.val
        let idx = storeScalarIdx.val
        scalarRegFileB.store idx s
        scalarRegFileD.store idx s
        scalarRegFileF.store idx s
        evictStatusB.store idx false
        -- Reclaim vector space
        when (scalarRegFileF.out `is` #vector) do
          dynamicAssert freeSlots.notFull
            "Scalarising reg file: freeSlots overflow"
          freeSlots.push2 (truncateCast (untag #vector scalarRegFileF.out))
          vecCountDecr2.pulse
        -- Track vector registers
        when opts.useDynRegSpill do
          let (warpId, regId) = storeScalarIdx.val
          ((vecMasks ! warpId) ! regId) <== false

  -- Expand scalar register to vector
  let expandScalar scalarReg offsets = 
        if opts.useAffine
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
    , loadScalarC = \idx -> do
        scalarRegFileC.load idx
    , loadScalarD = \idx -> do
        scalarRegFileD.load idx
    , outA =
        let isVector = delay false (scalarRegFileA.out `is` #vector) in
          if enSharedVecSpad
            then
              delay false isVector ?
                ( V.fromList [bank.out | bank <- vecSpadA]
                , old $ expandScalar (old $ untag #scalar scalarRegFileA.out)
                                     affineOffsetsA )
            else
              old $ isVector ?
                ( V.fromList [bank.out | bank <- vecSpadA]
                , expandScalar (old $ untag #scalar scalarRegFileA.out)
                               affineOffsetsA )
    , outB =
        let isVector = delay false (scalarRegFileB.out `is` #vector) in
          if enSharedVecSpad
            then
              delay false isVector ?
                ( V.fromList [bank.out | bank <- vecSpadB]
                , old $ expandScalar (old $ untag #scalar scalarRegFileB.out)
                                     affineOffsetsB )
            else
              old $ isVector ?
                ( V.fromList [bank.out | bank <- vecSpadB]
                , expandScalar (old $ untag #scalar scalarRegFileB.out)
                               affineOffsetsB )
    , evictedA = iterateN 2 (delay false) (scalarRegFileA.out `is` #evicted)
    , evictedB = iterateN 2 (delay false) (scalarRegFileB.out `is` #evicted)
    , loadEvictedStatus = \idx -> evictStatus.load idx
    , evictedStatus = iterateN 2 (delay false) evictStatus.out
    , scalarA =
        let isScalar = delay false (scalarRegFileA.out `is` #scalar)
            scalar = old (untag #scalar scalarRegFileA.out)
        in  delay none (Option isScalar scalar)
    , scalarB =
        let isScalar = delay false (scalarRegFileB.out `is` #scalar)
            scalar = old (untag #scalar scalarRegFileB.out)
        in  delay none (Option isScalar scalar)
    , scalarC =
        Option (scalarRegFileC.out `is` #scalar)
               (untag #scalar scalarRegFileC.out)
    , scalarD =
        Option (scalarRegFileD.out `is` #scalar)
               (untag #scalar scalarRegFileD.out)
    , store = \idx vec -> do
        store1 <== true
        storeIdx1 <== idx
        storeVec1 <== vec
        scalarRegFileE.load idx
        -- Determine stride for affine scalarisation
        when opts.useAffine do
          let v0 = vec ! (0::Int)
          let v1 = vec ! (1::Int)
          let diff = v1.val - v0.val
          let mask = fromInteger (2^SIMTAffineScalarisationBits - 1)
          storeStride1 <==
            if v0.valid .&&. v1.valid .&&. (diff .&. inv mask .==. 0)
              then truncateCast diff
              else 0
    , canStore = \idx ->
        inv $ orList [
                store1.val .&&. storeIdx1.val .==. idx
              , store2.val .&&. storeIdx2.val .==. idx
              ]
    , evict = \idx -> do
        store1 <== true
        storeIdx1 <== idx
        storeEvict1 <== true
        scalarRegFileE.load idx
    , storeScalar = \idx x -> do
        scalarRegFileF.load idx
        storeScalarGo <== true
        storeScalarIdx <== idx
        storeScalarVal <== x
    , storeLatency = 2
    , init = do
        initInProgress <== true
        freeSlots.clear
    , initInProgress = initInProgress.val
    , maxVecRegs = maxVecCount.val
    , numVecRegsUnused = fromIntegral opts.size - vecCount.val
    , numVecRegs = vecCount.val
    , totalVecRegs = totalVecCount.val
    , getVecMasks = 
        V.map (\mask -> pack (V.map (.val) mask)) vecMasks
    , sharedVecSpad =
        case opts.useSharedVecSpad of
          Nothing ->
            SharedVecSpad {
              vecSpadA = vecSpadB
            , vecSpadB = vecSpadA
            , freeSlots =
                SlotManager {
                  push1 = freeSlots.push3
                , push2 = freeSlots.push4
                , push3 = error "Free slots push method not available"
                , push4 = error "Free slots push method not available"
                , top1  = freeSlots.top2
                , top2  = error "Free slots top method not available"
                , pop1  = freeSlots.pop2
                , pop2  = error "Free slots pop method not available"
                , clear = return ()
                , notFull = freeSlots.notFull
                , notEmpty = freeSlots.notEmpty
                }
            }
          Just spad -> spad
    , stall = case opts.useSharedVecSpad of
                Nothing -> false
                Just spad -> loadStallWire.val
    }
