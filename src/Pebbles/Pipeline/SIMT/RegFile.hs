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

-- | Affine stride
data Stride = Stride (Bit 2) deriving (Generic, Bits, Interface, Cmp, FShow)
stride_0 = Stride 0
stride_1 = Stride 1
stride_2 = Stride 2
stride_4 = Stride 3

-- | Index to partial mask memory
type PartialMaskIdx = Bit SIMTCapRFLogNumPartialMasks

-- | This structure represents a unform or affine vector
data ScalarVal n =
  ScalarVal {
    val :: Bit n
    -- ^ Value
  , stride :: Stride
    -- ^ Stride between values of an affine vector;
    -- if stride is 0 then the vector is uniform
  , partial :: Option PartialMaskIdx
    -- ^ Pointer to mask (for partial scalarisation)
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
  , pipelineActive :: Bit 1
    -- ^ Is processor pipeline active?
  , useInitValOpt :: Bool
    -- ^ Use initial value optimisation
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

  -- Partial mask RAMs
  (partialRegFileA, partialRegFileB) ::
    (RAM PartialMaskIdx (Bit SIMTLanes),
     RAM PartialMaskIdx (Bit SIMTLanes)) <-
       if opts.useInitValOpt
         then makeQuadRAM
         else return (nullRAM, nullRAM)
  (partialRegFileE, partialRegFileF) ::
    (RAM PartialMaskIdx (Bit SIMTLanes),
     RAM PartialMaskIdx (Bit SIMTLanes)) <-
       if opts.useInitValOpt
         then makeQuadRAM
         else return (nullRAM, nullRAM)

  -- Partial mask slots
  partialSlots :: SlotManager PartialMaskIdx <-
    if opts.useInitValOpt
      then makeSlotManager SIMTCapRFLogNumPartialMasks
      else return nullSlotManager

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
  sampleCount :: Reg (Bit 32) <- makeReg 0

  -- Get lower bits for affine detection
  let getLower :: Bit regWidth -> Bit (SIMTLogLanes+2)
      getLower = unsafeSlice (SIMTLogLanes+1, 0)

  -- Get upper bits for affine detection
  let getUpper :: Bit regWidth -> Bit (regWidth-SIMTLogLanes-2)
      getUpper = unsafeSlice (valueOf @regWidth - 1, SIMTLogLanes+2)

  let enSharedVecSpad = 
        case opts.useSharedVecSpad of
          Nothing -> False
          Just spad -> True

  -- Helper functions
  -- ================

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
            , stride = stride_0
            , partial = none
            }
      let idx = unpack initIdx.val
      scalarRegFileB.store idx initScalarReg
      scalarRegFileD.store idx initScalarReg
      scalarRegFileF.store idx initScalarReg
      evictStatus.store idx false
      when (not enSharedVecSpad) do
        when (initIdx.val .<=. fromIntegral (opts.size - 1)) do
          freeSlots.push1 (truncateCast initIdx.val)
      when opts.useInitValOpt do
        when (initIdx.val .<=.
                fromIntegral (2^SIMTCapRFLogNumPartialMasks-1)) do
          let slot = truncateCast initIdx.val
          partialSlots.push1 slot
      when opts.useDynRegSpill do
        sequence_ [ sequence_ [ b <== false | b <- toList mask ]
                  | mask <- toList vecMasks ]
      initIdx <== initIdx.val - 1
      when (initIdx.val .==. 0) do
        vecCount <== 0
        sampleCount <== 0
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

  always do
    when (orList [vecCountIncr.val, vecCountDecr1.val
                                  , vecCountDecr2.val]) do
      vecCount <== vecCount.val + zeroExtend vecCountIncr.val
                                - zeroExtend vecCountDecr1.val
                                - zeroExtend vecCountDecr2.val

    when (opts.pipelineActive .&&. inv initInProgress.val) do
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

    when opts.useInitValOpt do
      let scalarA = untag #scalar scalarRegFileA.out
      let scalarB = untag #scalar scalarRegFileB.out
      partialRegFileA.load scalarA.partial.val
      partialRegFileB.load scalarB.partial.val

  -- Store path
  -- ==========

  -- Stage 1 pipeline registers
  store1 <- makeDReg false
  storeIdx1 <- makeReg dontCare
  storeVec1 <- makeReg dontCare
  storeEvict1 <- makeDReg false
  storeLeaderLane1 :: Reg (Bit SIMTLogLanes) <- makeReg dontCare
  storeLeaderVal1 :: Reg (Bit regWidth) <- makeReg dontCare

  -- Stage 2 pipeline registers
  store2 <- makeDReg false
  storeIdx2 <- makeReg dontCare
  storeVec2 <- makeReg dontCare
  storeEvict2 <- makeDReg false
  storeIsScalar2 <- makeReg dontCare
  storeScalarEntry2 <- makeReg dontCare
  storeStride2 <- makeReg dontCare
  storeLeaderVal2 <- makeReg dontCare
  storeInitVal2 :: Reg (Bit 1) <- makeReg false

  always do
    -- Stage 1
    when store1.val do
      -- Write mask
      let writeMask :: Bit SIMTLanes =
            fromBitList [item.valid | item <- toList storeVec1.val]
      -- Scalar reg before write
      let scalarReg = untag #scalar scalarRegFileE.out
      -- Validity of writes
      let anyValid = writeMask .!=. 0
      let allValid = writeMask .==. ones
      -- Are the upper bits of each active element the same?
      let isBaseEq = andList
             [ inv x.valid .||.
                 getUpper x.val .==. getUpper storeLeaderVal1.val
             | x <- toList storeVec1.val ]
      -- Is there a stride of 0?
      let isStride0 = andList
             [ inv x.valid .||.
                 getLower x.val .==. getLower storeLeaderVal1.val
             | x <- toList storeVec1.val ]
      -- Is there a stride of 1?
      let isStride1 = andList
             [ inv x.valid .||.
                 upper low .==. (upper lowLeader :: Bit 2) .&&.
                 lower low .==. (fromInteger laneId :: Bit SIMTLogLanes)
             | (x, laneId) <- zip (toList storeVec1.val) [0..]
             , let low = getLower x.val
             , let lowLeader = getLower storeLeaderVal1.val
             ]
      -- Is there a stride of 2?
      let isStride2 = andList
             [ inv x.valid .||.
                 upper low .==. (upper lowLeader :: Bit 1) .&&.
                 slice @SIMTLogLanes @1 low .==. fromInteger laneId .&&.
                 lower low .==. (0 :: Bit 1)
             | (x, laneId) <- zip (toList storeVec1.val) [0..]
             , let low = getLower x.val
             , let lowLeader = getLower storeLeaderVal1.val
             ]
      -- Is there a stride of 4?
      let isStride4 = andList
             [ inv x.valid .||.
                 upper low .==. (fromInteger laneId :: Bit SIMTLogLanes) .&&.
                 lower low .==. (0 :: Bit 2)
             | (x, laneId) <- zip (toList storeVec1.val) [0..]
             , let low = getLower x.val
             ]
      -- Chosen stride
      let stride =
            if opts.useAffine
              then priorityIf [
                     isStride0 --> stride_0
                   , isStride1 --> stride_1
                   , isStride2 --> stride_2
                   , isStride4 --> stride_4
                   ] dontCare
              else stride_0
      when opts.useInitValOpt do
        partialRegFileE.load scalarReg.partial.val
        storeInitVal2 <== isStride0 .&&.
          storeLeaderVal1.val .==. opts.regInitVal
      -- Compute new vector to write
      let writeVals :: Vec SIMTLanes (Bit regWidth) = fromList
            [ item.valid ? (item.val, scal)
            | (item, scal) <- zip
                (toList storeVec1.val)
                (toList (expandScalar opts.useAffine Nothing scalarReg)) ]
      -- Trigger next stage
      storeIsScalar2 <== andList
        [ inv storeEvict1.val
        , isBaseEq
        , if opts.useAffine
              then orList [isStride0, isStride1, isStride2, isStride4]
              else isStride0
        , anyValid
        ]
      storeScalarEntry2 <== scalarRegFileE.out
      storeIdx2 <== storeIdx1.val
      storeVec2 <== V.zipWith (\item writeVal -> Option item.valid writeVal)
                      storeVec1.val writeVals
      storeStride2 <== stride
      storeLeaderVal2 <== storeLeaderVal1.val
      storeEvict2 <== storeEvict1.val
      when (anyValid .||. storeEvict1.val) do
        store2 <== true
 
    -- Stage 2
    when store2.val do
      -- Write mask
      let writeMask :: Bit SIMTLanes =
            fromBitList [item.valid | item <- toList storeVec2.val]
      -- Was it a vector before this write?
      let wasVector = storeScalarEntry2.val `is` #vector
      -- Was it an evicted vector before this write?
      let wasEvicted = storeScalarEntry2.val `is` #evicted
      -- Was it a scalar before this write?
      let wasScalar = storeScalarEntry2.val `is` #scalar
      -- Next free slot
      let slot = freeSlots.top1

      -- For partial (init value) optimisation
      let scalarReg = untag #scalar storeScalarEntry2.val
      let allValid = writeMask .==. ones
      let wasUniform = wasScalar .&&.
                         (if opts.useAffine
                            then scalarReg.stride .==. stride_0
                            else true)
      let idempotent =
            wasUniform .&&. scalarReg.val .==. storeLeaderVal2.val
                       .&&. storeStride2.val .==. stride_0
      let allocatePartialMask = andList
            [ wasScalar
            , storeIsScalar2.val
            , inv allValid
            , inv idempotent
            , inv scalarReg.partial.valid
            , partialSlots.notEmpty
            , if storeInitVal2.val
                then true
                else scalarReg.stride .==. stride_0 .&&.
                       scalarReg.val .==. opts.regInitVal
            ]
      let maintainPartialMask = andList
            [ wasScalar
            , storeIsScalar2.val
            , scalarReg.partial.valid
            , inv allValid
            , storeInitVal2.val .||. idempotent
            {- We can transition from partial scalar to total scalar using
               the following conditions, but then need to change the
               definition of isScalar below
            , orList [
                storeInitVal2.val .&&.
                  ((writeMask .|. partialRegFileE.out) .!=. ones)
              , idempotent .&&. 
                  ((writeMask .|. inv partialRegFileE.out) .!=. ones)
              ]
            -}
            ]
      let releasePartialMask = andList
            [ wasScalar
            , scalarReg.partial.valid
            , inv maintainPartialMask
            ]
      when opts.useInitValOpt do
        when allocatePartialMask do
          partialSlots.pop1
        when releasePartialMask do
          partialSlots.push1 scalarReg.partial.val

      -- Is it a scalar or vector after this write?
      let isScalar = andList
            [ storeIsScalar2.val
            , allValid .||. idempotent .||.
                (if opts.useInitValOpt
                   then allocatePartialMask .||. maintainPartialMask
                   else false) ]
      let isVector = if SIMTRegFilePreventScalarDetection == 1
                       then true
                       else inv isScalar

{-
      when opts.useInitValOpt do
        when (fst storeIdx2.val .==. 0 .&&. isVector .&&. wasScalar) do
          display "writeMask=" (formatHex 8 writeMask)
                  " partialMask.valid=" scalarReg.partial.valid
                  " partialMask=" (formatHex 8 partialRegFileE.out)
                  " storeIsScalar=" storeIsScalar2.val
                  " storeInitVal=" storeInitVal2.val
                  " scalarVal=" (scalarReg.val)
                  " scalarStride=" (scalarReg.stride)
                  " initVal=" opts.regInitVal
                  " "(V.map (.val) storeVec2.val)
                  " maintainPartialMask=" maintainPartialMask
                  " storeLeaderVal=" storeLeaderVal2.val
                  " write stride=" storeStride2.val
-}

      -- Scalar <-> vector transitions
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
          let scalar = untag #scalar storeScalarEntry2.val
          sequence_
            [ when (item.valid .||. inv wasVector) do
                let writeVal =
                      if opts.useInitValOpt
                        then (inv item.valid .&&. wasScalar .&&.
                                scalar.partial.valid .&&. useInit) ?
                                  (opts.regInitVal, item.val)
                        else item.val
                 in bank.store spadAddr writeVal
            | (bank, item, useInit) <-
                zip3 vecSpadA (toList storeVec2.val)
                              (toBitList partialRegFileE.out) ]
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
          let storePartialSlot =
                if allocatePartialMask
                  then Option true partialSlots.top1
                  else Option (wasScalar .&&. scalarReg.partial.valid .&&.
                                 maintainPartialMask)
                              scalarReg.partial.val
          let writeStride =
                if opts.useInitValOpt
                  then (if storePartialSlot.valid .&&. storeInitVal2.val
                          then scalarReg.stride else storeStride2.val)
                  else storeStride2.val
          let writeBase = 
                if opts.useInitValOpt
                  then (if storePartialSlot.valid .&&. storeInitVal2.val
                          then scalarReg.val else storeLeaderVal2.val)
                  else storeLeaderVal2.val
          let scalarVal =
                ScalarVal {
                  val = writeBase
                , stride = writeStride
                , partial = storePartialSlot
                }
          let writeVal = storeEvict2.val ?
                (tag #evicted (), tag #scalar scalarVal)
          
          scalarRegFileA.store storeIdx2.val writeVal
          scalarRegFileC.store storeIdx2.val writeVal
          scalarRegFileE.store storeIdx2.val writeVal
          evictStatus.store storeIdx2.val storeEvict2.val

          when opts.useInitValOpt do
            let currentMask =
                  if allocatePartialMask
                    then 0
                    else partialRegFileE.out
            let newMask =
                  if storeInitVal2.val
                    then currentMask .|. writeMask
                    else currentMask .&. inv writeMask
            when storePartialSlot.valid do
              partialRegFileA.store storePartialSlot.val newMask
              partialRegFileE.store storePartialSlot.val newMask

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
        dynamicAssert (inv storeScalarVal.val.partial.valid)
          "storeScalar does not support partial vectors"
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
        -- Reclaim partial scalar space
        when opts.useInitValOpt do
          when (scalarRegFileF.out `is` #scalar) do
            let scalar = untag #scalar scalarRegFileF.out
            when scalar.partial.valid do
              partialSlots.push2 scalar.partial.val
        -- Track vector registers
        when opts.useDynRegSpill do
          let (warpId, regId) = storeScalarIdx.val
          ((vecMasks ! warpId) ! regId) <== false

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
        let isVector = delay false (scalarRegFileA.out `is` #vector)
            scalar = old (untag #scalar scalarRegFileA.out)
            mask = scalar.partial.valid ? (partialRegFileA.out, 0)
            mb_mask = if opts.useInitValOpt
                        then Just (mask, opts.regInitVal)
                        else Nothing
        in if enSharedVecSpad
             then
               delay false isVector ?
                 ( V.fromList [bank.out | bank <- vecSpadA]
                 , old $ expandScalar opts.useAffine mb_mask scalar )
             else
               old $ isVector ?
                 ( V.fromList [bank.out | bank <- vecSpadA]
                 , expandScalar opts.useAffine mb_mask scalar )
    , outB =
        let isVector = delay false (scalarRegFileB.out `is` #vector)
            scalar = old (untag #scalar scalarRegFileB.out)
            mask = scalar.partial.valid ? (partialRegFileB.out, 0)
            mb_mask = if opts.useInitValOpt
                        then Just (mask, opts.regInitVal)
                        else Nothing
        in if enSharedVecSpad
             then
               delay false isVector ?
                 ( V.fromList [bank.out | bank <- vecSpadB]
                 , old $ expandScalar opts.useAffine mb_mask scalar )
             else
               old $ isVector ?
                 ( V.fromList [bank.out | bank <- vecSpadB]
                 , expandScalar opts.useAffine mb_mask scalar )
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
        -- Determine leader
        let oneHotIdx :: Bit SIMTLanes =
              firstHot $ fromBitList $ map (.valid) (V.toList vec)
        let idx = binaryEncode oneHotIdx
        storeLeaderLane1 <== idx
        storeLeaderVal1 <== select (zip (toBitList oneHotIdx)
                                   (map (.val) $ V.toList vec))
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
        partialSlots.clear
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

-- Helper functions
-- ================

-- Check if incrementing by val is compatible with given stride
affineAlignCheck :: KnownNat n => Bit n -> Stride -> Bit 1
affineAlignCheck val s = 
   select [  
     s .==. stride_0 --> true
   , s .==. stride_1 --> c1
   , s .==. stride_2 --> c1 .&&. c2
   , s .==. stride_4 --> c1 .&&. c2 .&&. c3
   ]
  where
    c1 = truncateCast val .==. (0 :: Bit SIMTLogLanes)
    c2 = unsafeAt SIMTLogLanes val .==. false
    c3 = unsafeAt (SIMTLogLanes+1) val .==. false

-- Expand compressed affine vector using lane id
expandAffine :: forall regWidth. KnownNat regWidth
             => Bit SIMTLogLanes
             -> ScalarVal regWidth
             -> Bit regWidth
expandAffine i s = unsafeBitCast
  (unsafeSlice ( valueOf @regWidth - 1
               , SIMTLogLanes+2 ) s.val # expanded)
  where
    expanded :: Bit (SIMTLogLanes+2) =
      select
        [ s.stride .==. stride_0 -->
            unsafeSlice (SIMTLogLanes+1, 0) s.val
        , s.stride .==. stride_1 -->
            unsafeSlice (SIMTLogLanes+1, SIMTLogLanes) s.val # i
        , s.stride .==. stride_2 -->
            unsafeAt (SIMTLogLanes+1) s.val # i # (0 :: Bit 1)
        , s.stride .==. stride_4 -->
            i # (0 :: Bit 2) ]

-- Expand scalar register to vector
expandScalar :: forall regWidth. KnownNat regWidth
             => Bool
             -> Maybe (Bit SIMTLanes, Bit regWidth)
             -> ScalarVal regWidth
             -> Vec SIMTLanes (Bit regWidth)
expandScalar useAffine useMask scalarReg =
  case useMask of
    Nothing -> V.fromList [ getLane (fromInteger i) scalarReg
                          | i <- [0 .. SIMTLanes-1 ] ]
    Just (mask, initVal) ->
      let initScalarVal = ScalarVal { val = initVal
                                    , stride = stride_0
                                    , partial = none  }
       in V.fromList [ getLane (fromInteger i)
                               (if scalarReg.partial.valid .&&. init
                                  then initScalarVal else scalarReg)
                     | (i, init) <- zip [0..] (toBitList mask) ]
  where
    getLane i s = if useAffine then expandAffine i s else s.val
