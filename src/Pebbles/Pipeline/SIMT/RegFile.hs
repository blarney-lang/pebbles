{-# LANGUAGE RecordWildCards #-}

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

-- | This structure represents a unform or affine vector
data ScalarVal n =
  ScalarVal {
    val :: Bit n
    -- ^ Value
  , stride :: Stride
    -- ^ Stride between values of an affine vector;
    -- if stride is 0 then the vector is uniform
  , mask :: PartialMask
  }
  deriving (Generic, Interface, Bits, FShow)

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
  , loadVecMask :: Bit SIMTLogWarps -> Action ()
  , getVecMask :: Bit 32
    -- ^ Which registers for each warp are vectors?
  , sharedVecSpad :: SharedVecSpad t_logSize regWidth
    -- ^ Use shared vector scratchpad
  , stall :: Bit 1
    -- ^ Stall load pipeline
  , triggerStall :: Action ()
    -- ^ Explicitly pulse the stall output wire
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
    , getVecMask = ones
    , loadVecMask = \_ -> return ()
    , sharedVecSpad = error "Null SIMT regfile does not produce vec spad"
    , stall = false
    , triggerStall = return ()
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
    , getVecMask = ones
    , loadVecMask = \_ -> return ()
    , sharedVecSpad = error "SIMT regfile does not produce vec spad"
    , stall = false
    , triggerStall = return ()
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
  , sharePortB :: Bool
    -- ^ Share SRF port between operand B read and store read
  }

-- | Scalarising implementation
makeSIMTScalarisingRegFile :: forall t_logSize regWidth.
     (KnownNat t_logSize, KnownNat regWidth)
  => SIMTScalarisingRegFileConfig t_logSize regWidth
     -- ^ Config options
  -> Module (SIMTRegFile t_logSize regWidth)
makeSIMTScalarisingRegFile opts = do

  staticAssert (not (opts.sharePortB && opts.useScalarUnit)) $
     "makeSIMTScalarisingRegFile: sharePortB and useScalarUnit " ++
     "cannot be (yet) be used together"

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
     RAM SIMTRegFileIdx (ScalarReg regWidth)) <- 
       if opts.sharePortB
         then return ( case scalarRegFileB of
                         RAM { .. } -> RAM { store = \_ _ -> return (), .. }
                     , nullRAM )
         else makeQuadRAM

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
  (vecMasksA, vecMasksB) ::
    (RAM (Bit SIMTLogWarps) (Bit 32),
     RAM (Bit SIMTLogWarps) (Bit 32)) <-
       if opts.useDynRegSpill
         then makeQuadRAM
         else return (nullRAM, nullRAM)
  (vecMasksC, vecMasksD) ::
    (RAM (Bit SIMTLogWarps) (Bit 32),
     RAM (Bit SIMTLogWarps) (Bit 32)) <-
       if opts.useDynRegSpill && opts.useScalarUnit
         then makeQuadRAM
         else return (nullRAM, nullRAM)

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
            , mask = zeroMask
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
        vecMasksB.store (truncateCast initIdx.val) 0
        vecMasksD.store (truncateCast initIdx.val) 0
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

  -- External trigger to stall pipeline (to handle shared SRF port)
  explicitStallWire <- makePulseWire

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

  -- Store path
  -- ==========

  -- Stage 1 pipeline registers
  store1 <- makeDReg false
  storeIdx1 <- makeReg dontCare
  storeVec1 <- makeReg dontCare
  storeEvict1 <- makeDReg false
  storeLeaderLane1 :: Reg (Bit SIMTLogLanes) <- makeReg dontCare
  writeSRF <- makeWire none

  -- Stage 2 pipeline registers
  store2 <- makeDReg false
  storeIdx2 <- makeReg dontCare
  storeVec2 <- makeReg dontCare
  storeEvict2 <- makeDReg false
  storeScalar2 <- makeReg dontCare
  storeScalarEntry2 <- makeReg dontCare

  always do
    -- Stage 1
    when store1.val do
      let storeLeaderVal1 = if opts.useInitValOpt
            then ((storeVec1.val) ! storeLeaderLane1.val).val
            else (V.head storeVec1.val).val
      -- Write mask
      let writeMask :: Bit SIMTLanes =
            fromBitList [item.valid | item <- toList storeVec1.val]
      -- Validity of writes
      let anyValid = writeMask .!=. 0
      let allValid = writeMask .==. ones
      -- Are the upper bits of each active element the same?
      let isBaseEq = andList
             [ inv x.valid .||.
                 getUpper x.val .==. getUpper storeLeaderVal1
             | x <- toList storeVec1.val ]
      -- Is there a stride of 0?
      let isStride0 = andList
             [ inv x.valid .||.
                 getLower x.val .==. getLower storeLeaderVal1
             | x <- toList storeVec1.val ]
      -- Is there a stride of 1?
      let isStride1 = andList
             [ inv x.valid .||.
                 upper low .==. (upper lowLeader :: Bit 2) .&&.
                 lower low .==. (fromInteger laneId :: Bit SIMTLogLanes)
             | (x, laneId) <- zip (toList storeVec1.val) [0..]
             , let low = getLower x.val
             , let lowLeader = getLower storeLeaderVal1
             ]
      -- Is there a stride of 2?
      let isStride2 = andList
             [ inv x.valid .||.
                 upper low .==. (upper lowLeader :: Bit 1) .&&.
                 slice @SIMTLogLanes @1 low .==. fromInteger laneId .&&.
                 lower low .==. (0 :: Bit 1)
             | (x, laneId) <- zip (toList storeVec1.val) [0..]
             , let low = getLower x.val
             , let lowLeader = getLower storeLeaderVal1
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
      -- Scalar reg before write
      let scalarReg = untag #scalar scalarRegFileE.out
      -- Is the write scalar?
      let isWriteScalar = andList
            [ isBaseEq
            , if opts.useAffine
                then orList [isStride0, isStride1, isStride2, isStride4]
                else isStride0
            ]
      when isWriteScalar do
        let isWriteInit =
              storeLeaderVal1 .==. opts.regInitVal .&&. isStride0
        if allValid
          then do
            writeSRF <== some ScalarVal
              { val    = storeLeaderVal1
              , stride = stride
              , mask   = if isWriteInit then fullMask else zeroMask
              }
          else do
            -- TODO: is val being stored equal to val already there?
            -- TODO: expand mask on previous cycle?
            -- TODO: check Fmax; introduce buffer in writeback?
            when opts.useInitValOpt do
              let currentMask = expandPartialMask scalarReg.mask
              when (scalarRegFileE.out `is` #scalar) do
                let newMask = compressPartialMask (if isWriteInit
                                then currentMask .|. writeMask
                                else currentMask .&. inv writeMask)
                when newMask.valid do
                  if isWriteInit
                    then do
                      writeSRF <== some ScalarVal
                        { val    = scalarReg.val
                        , stride = scalarReg.stride
                        , mask   = newMask.val
                        }
                    else do
                      when ((writeMask .&. currentMask) .==. writeMask) do
                        writeSRF <== some ScalarVal
                          { val    = storeLeaderVal1
                          , stride = stride
                          , mask   = newMask.val
                         }
      -- Is it a scalar write?
      storeScalar2 <== writeSRF.val
      -- Compute new vector to write
      let mb_mask = if opts.useInitValOpt
                      then Just (scalarReg.mask, opts.regInitVal)
                      else Nothing
      -- TODO: expandScalar will expand the mask but we have
      -- already done that here, leading to duplicated logic
      let writeVals :: Vec SIMTLanes (Bit regWidth) = fromList
            [ item.valid ? (item.val, scal)
            | (item, scal) <-
                zip (toList storeVec1.val)
                    (toList (expandScalar opts.useAffine mb_mask scalarReg)) ]
      -- Trigger next stage
      storeScalarEntry2 <== scalarRegFileE.out
      storeIdx2 <== storeIdx1.val
      storeVec2 <== V.zipWith (\item writeVal -> Option item.valid writeVal)
                      storeVec1.val writeVals
      storeEvict2 <== storeEvict1.val
      when (anyValid .||. storeEvict1.val) do
        store2 <== true
      let (warpId, _) = storeIdx1.val
      vecMasksB.load warpId
 
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
                       else inv storeScalar2.val.valid
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
                bank.store spadAddr item.val
            | (bank, item) <-
                zip vecSpadA (toList storeVec2.val) ]
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
          let oldScalarVal = untag #scalar storeScalarEntry2.val
          let scalarVal = storeScalar2.val.val
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
        let rmask = 1 .<<. regId
        let newMask = if isVec then vecMasksB.out .|. rmask
                               else vecMasksB.out .&. inv rmask
        vecMasksA.store warpId newMask
        vecMasksC.store warpId newMask

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
          let newMask = vecMasksC.out .&. inv (1 .<<. regId)
          vecMasksB.store warpId newMask
          vecMasksD.store warpId newMask

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
            mb_mask = if opts.useInitValOpt
                        then Just (scalar.mask, opts.regInitVal)
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
            mb_mask = if opts.useInitValOpt
                        then Just (scalar.mask, opts.regInitVal)
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
        if opts.useInitValOpt
          then do
            let oneHotIdx :: Bit SIMTLanes =
                  firstHot $ fromBitList $ map (.valid) (V.toList vec)
            storeLeaderLane1 <== binaryEncode oneHotIdx
          else do
            storeLeaderLane1 <== 0
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
        vecMasksC.load (fst idx)
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
    , loadVecMask = vecMasksA.load
    , getVecMask = vecMasksA.out
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
    , stall = (if opts.sharePortB then explicitStallWire.val else false)
                 .||. case opts.useSharedVecSpad of
                        Nothing -> false
                        Just spad -> loadStallWire.val
    , triggerStall = if opts.sharePortB
                       then explicitStallWire.pulse else return ()
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
             -> Maybe (PartialMask, Bit regWidth)
             -> ScalarVal regWidth
             -> Vec SIMTLanes (Bit regWidth)
expandScalar useAffine useMask scalarReg =
  case useMask of
    Nothing -> V.fromList [ getLane (fromInteger i) scalarReg
                          | i <- [0 .. SIMTLanes-1 ] ]
    Just (mask, initVal) ->
      let initScalarVal = ScalarVal { val = initVal
                                    , stride = stride_0
                                    , mask = zeroMask }
       in V.fromList [ getLane (fromInteger i)
                               (if init then initScalarVal else scalarReg)
                     | (i, init) <- zip [0..]
                                        (toBitList (expandPartialMask mask)) ]
  where
    getLane i s = if useAffine then expandAffine i s else s.val

-- Compressed mask for partial scalarisation
-- =========================================

type PartialMask = Bit SIMTLanes

zeroMask :: PartialMask
zeroMask = 0

fullMask :: PartialMask
fullMask = ones

isMaskZero :: PartialMask -> Bit 1
isMaskZero m = m .==. 0

expandPartialMask :: PartialMask -> Bit SIMTLanes
expandPartialMask m = m

compressPartialMask :: Bit SIMTLanes -> Option PartialMask
compressPartialMask bits = Option true bits
