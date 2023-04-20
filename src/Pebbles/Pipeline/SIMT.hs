module Pebbles.Pipeline.SIMT
  ( -- Pipeline configuration
    SIMTPipelineConfig(..)
    -- Pipeline inputs and outputs
  , SIMTPipelineIns(..)
  , SIMTPipelineOuts(..)
    -- Instruction info for multi-cycle instructions
  , SIMTPipelineInstrInfo(..)
    -- Pipeline module
  , makeSIMTPipeline
  ) where

-- Simple 32-bit SIMT pipeline with a configurable number of warps and
-- warp size.
--
-- There are 7 pipeline stages:
--
--  0. Warp Scheduling
--  1. Active Thread Selection (consists of 2 sub-stages)
--  2. Instruction Fetch
--  3. Operand Fetch
--  4. Operand Latch (one or more sub-stages)
--  5. Execute (& Thread Suspension)
--  6. Writeback (& Thread Resumption)
--
-- Instructions are suspended in the Execute stage if they cannot
-- complete within a clock cycle.  Suspended instructions are resumed
-- in the Writeback stage.  Resumptions can be out-of-order with
-- respect to suspensions, and there is no requirement for threads in
-- a warp to resume at the same time.  This flexibility is achieved by
-- maininting a "suspension bit" register for every thread.
-- Currently, if a warp is scheduled that contains any suspended
-- threads, then a bubble passes through the pipeline and the warp
-- will be tried again later.  In future, we might attempt to avoid
-- scheduling a warp that contains a suspended thread.
--
-- Other considerations for future: (1) give retried instructions a
-- higher priority than new instructions in the warp scheduler; (2)
-- only pay cost of active thread selection on a branch/jump.

-- SoC configuration
#include <Config.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.Stream
import Blarney.BitScan
import Blarney.PulseWire
import Blarney.SourceSink
import Blarney.TaggedUnion
import Blarney.QuadPortRAM
import Blarney.TypeFamilies
import Blarney.Interconnect
import Blarney.Vector qualified as V
import Blarney.Vector (Vec, fromList, toList)

-- General imports
import Data.List
import Data.Proxy
import Data.Maybe
import qualified Data.Map as Map
import Control.Applicative hiding (some)

-- Pebbles imports
import Pebbles.Util.List
import Pebbles.Util.Counter
import Pebbles.Pipeline.Interface
import Pebbles.Pipeline.SIMT.Management
import Pebbles.Pipeline.SIMT.RegFile
import Pebbles.Memory.Interface
import Pebbles.Memory.DRAM.Interface
import Pebbles.Memory.CoalescingUnit
import Pebbles.CSRs.TrapCodes
import Pebbles.CSRs.Custom.SIMTDevice

-- CHERI imports
import CHERI.CapLib

-- | Info about multi-cycle instructions issued by pipeline
data SIMTPipelineInstrInfo =
  SIMTPipelineInstrInfo {
    destReg :: RegId
    -- ^ Destination register
  , warpId :: Bit SIMTLogWarps
    -- ^ Warp that issued the instruction
  , regFileId :: RegFileId
    -- ^ Destination register file
  }
  deriving (Generic, Interface, Bits)

-- | SIMT pipeline configuration
data SIMTPipelineConfig tag =
  SIMTPipelineConfig {
    instrMemInitFile :: Maybe String
    -- ^ Instruction memory initialisation file
  , instrMemLogNumInstrs :: Int
    -- ^ Instuction memory size (in number of instructions)
  , instrMemBase :: Integer
    -- ^ Base address of instruction memory in memory map
  , enableStatCounters :: Bool
    -- ^ Are stat counters enabled?
  , checkPCCFunc :: Maybe (Cap -> [(Bit 1, TrapCode)])
    -- ^ When CHERI is enabled, function to check PCC
  , useSharedPCC :: Bool
    -- ^ When CHERI enabled, use shared PCC (meta-data) per kernel
  , decodeStage :: [(String, tag)]
    -- ^ Decode table
  , executeStage :: [State -> Module ExecuteStage]
    -- ^ List of execute stages, one per lane
    -- The size of this list must match the warp size
  , simtPushTag :: tag
  , simtPopTag :: tag
    -- ^ Mnemonics for SIMT explicit convergence instructions
  , useRegFileScalarisation :: Bool
    -- ^ Use scalarising register file?
  , useAffineScalarisation :: Bool
    -- ^ Use affine scalarisation, or plain uniform scalarisation?
  , useCapRegFileScalarisation :: Bool
    -- ^ Use scalarising register file for capabilities?
  , useScalarUnit :: Bool
    -- ^ Use dedicated scalar unit for parallel scalar/vector execution?
  , scalarUnitAllowList :: [tag]
    -- ^ A list of instructions that can execute on the scalar unit
  , scalarUnitAffineAdd :: Maybe tag
    -- ^ Optionally replace add instr with built-in affine add instr
  , scalarUnitAffineCMove :: Maybe tag
    -- ^ Optionally replace cmove instr with built-in affine version
  , scalarUnitAffineCIncOffset :: Maybe tag
    -- ^ Optionally replace cincoffset instr with built-in affine version
  , scalarUnitDecodeStage :: [(String, tag)]
    -- ^ Decode table for scalar unit
  , scalarUnitExecuteStage :: State -> Module ExecuteStage
    -- ^ Execute stage for scalar unit
  , regSpillBaseAddr :: Integer
    -- ^ Base address of register spill region in DRAM
  , useLRUSpill :: Bool
    -- ^ Prefer to spill registers that are not recently used
  , useRRSpill :: Bool
    -- ^ Round robin spill strategy
  , useSharedVectorScratchpad :: Bool
    -- ^ Share vector scatchpad between int and cap reg files?
  , usesCap :: [tag]
    -- ^ Instructions that use cap meta-data of register operands
  }

-- | SIMT pipeline inputs
data SIMTPipelineIns =
  SIMTPipelineIns {
      simtMgmtReqs :: Stream SIMTReq
      -- ^ Stream of pipeline management requests
    , simtWarpCmdWire :: Wire WarpCmd
      -- ^ When this wire is active, the warp currently in the execute
      -- stage (assumed to be converged) is issuing a warp command
    , simtResumeReqs :: Stream (SIMTPipelineInstrInfo,
                                  Vec SIMTLanes (Option ResumeReq))
      -- ^ Resume requests for multi-cycle instructions (vector pipeline)
    , simtScalarResumeReqs :: Stream (SIMTPipelineInstrInfo, ResumeReq)
      -- ^ Resume requests for multi-cycle instructions (scalar pipeline)
    , simtDRAMStatSigs :: DRAMStatSigs
      -- ^ For DRAM stat counters
    , simtMemReqs :: Vec SIMTLanes (Sink MemReq)
      -- ^ Memory request path
    , simtCoalStats :: CoalUnitPerfStats
      -- ^ For coalescing unit stat counters
  }

-- | SIMT pipeline outputs
data SIMTPipelineOuts =
  SIMTPipelineOuts {
      simtMgmtResps :: Stream SIMTResp
      -- ^ Stream of pipeline management responses
    , simtCurrentWarpId :: Bit 32
      -- ^ Warp id of instruction currently in execute stage
    , simtKernelAddr :: Bit 32
      -- ^ Address of kernel closure as set by CPU
    , simtInstrInfo :: SIMTPipelineInstrInfo
      -- ^ Info for instruction currently in execute stage (vector pipeline)
    , simtScalarInstrInfo :: SIMTPipelineInstrInfo
      -- ^ Info for instruction currently in execute stage (scalar pipeline)
    , simtScalarisedOpB :: ScalarisedOperand
      -- ^ Scalarised operand B of instruction currently in vector
      -- pipeline's execute stage
  }

-- | Per-thread state
data SIMTThreadState =
  SIMTThreadState {
    simtPC :: Bit 32
    -- ^ Program counter
  , simtNestLevel :: Bit SIMTLogMaxNestLevel
    -- ^ SIMT divergence nesting level
  , simtRetry :: Bit 1
    -- ^ The last thing this thread did was a retry
  }
  deriving (Generic, Bits, Interface)

-- | SIMT pipeline module
makeSIMTPipeline :: Tag tag =>
     SIMTPipelineConfig tag
     -- ^ SIMT configuration options
  -> SIMTPipelineIns
     -- ^ SIMT pipeline inputs
  -> Module SIMTPipelineOuts
     -- ^ SIMT pipeline outputs
makeSIMTPipeline c inputs =
  -- Lift some parameters to the type level
  liftNat (c.instrMemLogNumInstrs) \(_ :: Proxy t_logInstrs) -> do

    -- Sanity check
    staticAssert (SIMTLanes == genericLength c.executeStage)
      "makeSIMTPipeline: warp size does not match number of execute units"

    -- Is CHERI enabled?
    let enableCHERI = isJust c.checkPCCFunc

    -- Is dynamic register file spilling logic enabled?
    -- See Note [Dynamic register spilling]
    let enableRegSpill = SIMTRegFileSize < SIMTWarps*32
    let enableCapSpill = enableCHERI && SIMTCapRegFileSize < SIMTWarps*32
    let enableSpill = enableRegSpill || enableCapSpill

    -- Compute field selector functions from decode table
    let selMap = matchSel (c.decodeStage)

    -- Functions for extracting register ids from an instruction
    let srcA :: Instr -> RegId = getBitFieldSel selMap "rs1"
    let srcB :: Instr -> RegId = getBitFieldSel selMap "rs2"
    let dst  :: Instr -> RegId = getBitFieldSel selMap "rd"

    -- Queue of active warps for vector pipeline
    warpQueue :: [Reg (Bit 1)] <- replicateM SIMTWarps (makeReg false)

    -- Track number of warps in scalar pipeline
    -- (Max val set to half num warps for load balancing)
    scalarUnitWarpCount :: Counter SIMTLogWarps <-
      makeCounter (fromInteger (SIMTWarps `div` 2))

    -- One block RAM of thread states per lane
    (stateMemsA, stateMemsB) :: ([RAM (Bit SIMTLogWarps) SIMTThreadState],
                                 [RAM (Bit SIMTLogWarps) SIMTThreadState]) <-
      unzip <$> replicateM SIMTLanes makeQuadRAM

    -- One program counter capability RAM (meta-data only) per lane
    (pccMemsA, pccMemsB) :: ([RAM (Bit SIMTLogWarps) CapMemMeta],
                             [RAM (Bit SIMTLogWarps) CapMemMeta]) <-
      unzip <$> (replicateM SIMTLanes $
                   if enableCHERI && not c.useSharedPCC
                     then makeQuadRAM
                     else return (nullRAM, nullRAM))

    -- Instruction memory
    (instrMemA, instrMemB) ::
      (RAM (Bit t_logInstrs) Instr, RAM (Bit t_logInstrs) Instr) <-
        makeQuadRAMCore c.instrMemInitFile

    -- Suspension bit for each thread
    suspBits :: [[Reg (Bit 1)]] <-
      replicateM SIMTLanes (replicateM SIMTWarps (makeReg false))

    -- Register file load latency (for vector pipeline)
    let loadLatency =
          if c.useCapRegFileScalarisation || c.useRegFileScalarisation
            then simtScalarisingRegFile_loadLatency
            else if enableCHERI then 2 else 1

    -- Is the pipeline active?
    pipelineActive :: Reg (Bit 1) <- makeReg false

    -- Register file
    regFile :: SIMTRegFile (Log2Ceil SIMTRegFileSize) 33 <-
      if c.useRegFileScalarisation
        then makeSIMTScalarisingRegFile
               SIMTScalarisingRegFileConfig {
                 useAffine = c.useAffineScalarisation
               , useScalarUnit = c.useScalarUnit
               , regInitVal = 0
               , size = SIMTRegFileSize
               , useDynRegSpill =
                   SIMTRegFileSize < SIMTWarps * 32
               , useSharedVecSpad = Nothing
               , pipelineActive = pipelineActive.val
               }
        else makeSIMTRegFile
               SIMTRegFileConfig {
                 loadLatency = loadLatency
               , regInitVal = Nothing
               }

    -- Capability register file (meta-data only)
    capRegFile :: SIMTRegFile (Log2Ceil SIMTCapRegFileSize) CapMemMetaWidth <-
      if enableCHERI
        then
          if c.useCapRegFileScalarisation
            then makeSIMTScalarisingRegFile
                   SIMTScalarisingRegFileConfig {
                     useAffine = False
                   , useScalarUnit = c.useScalarUnit
                   , regInitVal = nullCapMemMetaVal
                   , size = SIMTCapRegFileSize
                   , useDynRegSpill =
                       SIMTCapRegFileSize < SIMTWarps * 32
                   , useSharedVecSpad =
#if SIMTUseSharedVecScratchpad
                         Just regFile.sharedVecSpad
#else
                         Nothing
#endif
                   , pipelineActive = pipelineActive.val
                   }
            else makeSIMTRegFile
                   SIMTRegFileConfig {
                     loadLatency = loadLatency
                   , regInitVal = Just nullCapMemMetaVal
                   }
        else makeNullSIMTRegFile

    -- Scalar prediction table: for each instruction in the
    -- instruction memory, was the instruction scalarisable the
    -- last time it was executed?
    (scalarTableA, scalarTableB) ::
      (RAM (Bit t_logInstrs) (Bit 1),
       RAM (Bit t_logInstrs) (Bit 1)) <-
         if c.useScalarUnit then makeQuadRAM else return (nullRAM, nullRAM)

    -- Barrier bit for each warp
    barrierBits :: [Reg (Bit 1)] <- replicateM SIMTWarps (makeReg 0)

    -- Count of number of warps in a barrier
    -- (Not read when all warps in barrier, so overflow not a problem)
    barrierCount :: Counter SIMTLogWarps <- makeCounter dontCare

    -- Trigger for each stage
    go0 :: Reg (Bit 1) <- makeDReg false
    go1 :: Reg (Bit 1) <- makeDReg false
    go4 :: Reg (Bit 1) <- makeDReg false

    -- Warp id register, for each stage
    warpId1 :: Reg (Bit SIMTLogWarps) <- makeReg dontCare
    warpId4 :: Reg (Bit SIMTLogWarps) <- makeReg dontCare

    -- Thread state, for each stage
    state4 :: Reg SIMTThreadState <- makeReg dontCare

    -- Active thread mask
    activeMask2b :: Reg (Bit SIMTLanes) <- makeReg dontCare
    activeMask4 :: Reg (Bit SIMTLanes) <- makeReg dontCare

    -- Instruction register for each stage
    instr4 :: Reg (Bit 32) <- makeReg dontCare

    -- Is any thread in the current warp suspended?
    isSusp4 :: Reg (Bit 1) <- makeReg dontCare

    -- Insert warp back into warp queue at end of pipeline?
    rescheduleWarp6 :: Reg (Bit 1) <- makeDReg false

    -- Global exception register for entire core
    excGlobal :: Reg (Bit 1) <- makeReg false

    -- Program counter at point of exception
    excPC :: Reg (Bit 32) <- makeReg dontCare

    -- Per-lane exception register
    excLocals :: [Reg (Bit 1)] <- replicateM SIMTLanes (makeReg false)

    -- Kernel response queue (indicates to CPU when kernel has finished)
    kernelRespQueue :: Queue SIMTResp <- makeShiftQueue 1

    -- Track how many warps have terminated
    completedWarps :: Reg (Bit SIMTLogWarps) <- makeReg 0

    -- Track kernel success/failure
    kernelSuccess :: Reg (Bit 1) <- makeReg true

    -- Per-warp program counter capability registers, if shared PCC enabled
    (pccSharedA, pccSharedB) :: (RAM (Bit SIMTLogWarps) CapPipe,
                                 RAM (Bit SIMTLogWarps) CapPipe) <-
      if enableCHERI && c.useSharedPCC
        then makeQuadRAM
        else return (nullRAM, nullRAM)

    -- Basic stat counters
    cycleCount :: Reg (Bit 32) <- makeReg 0
    instrCount :: Reg (Bit 32) <- makeReg 0

    -- Stat counter for scalarisable instructions (if scalar unit
    -- disabled) or scalarised instructions (if scalar unit enabled)
    scalarisableInstrCount :: Reg (Bit 32) <- makeReg 0

    -- Count pipeline bubbles for perforance stats
    retryCount :: Reg (Bit 32) <- makeReg 0
    suspCount :: Reg (Bit 32)<- makeReg 0
    scalarSuspCount :: Reg (Bit 32) <- makeReg 0
    scalarAbortCount :: Reg (Bit 32) <- makeReg 0

    -- Count DRAM accesses for performance stats
    dramAccessCount :: Reg (Bit 32) <- makeReg 0

    -- Count coalescing unit store buffer load hit/miss
    sbLoadHitCount :: Reg (Bit 32) <- makeReg 0
    sbLoadMissCount :: Reg (Bit 32) <- makeReg 0
    sbCapLoadHitCount :: Reg (Bit 32) <- makeReg 0
    sbCapLoadMissCount :: Reg (Bit 32) <- makeReg 0

    -- Triggers from each execute unit to increment instruction count
    incInstrCountRegs <- replicateM SIMTLanes (makeDReg false)
    incScalarInstrCount <- makeDReg false
    incRetryCount <- makeDReg false
    incSuspCount <- makeDReg false
    incScalarSuspCount <- makeDReg false
    incScalarAbortCount <- makeDReg false

    -- Indicates that current instruction is scalarisable
    instrScalarisable5 <- makeReg false

    -- Scalar unit warp queue
    scalarQueue :: [Reg (Bit 1)] <- replicateM SIMTWarps (makeReg false)

    -- Function to convert from 32-bit PC to instruction address
    let toInstrAddr :: Bit 32 -> Bit t_logInstrs =
          \pc -> truncateCast (slice @31 @2 pc)

    -- For each pipeline stage, is it spilling a register for dynamic spilling?
    spill0 :: Reg (Bit 1) <- makeDReg false
    spill1 :: Reg (Bit 1) <- makeDReg false
    spill4 :: Reg (Bit 1) <- makeDReg false

    -- For each pipeline stage, is it spilling from int or cap reg file
    spillFrom0 :: Reg (Bit 1) <- makeReg dontCare
    spillFrom1 :: Reg (Bit 1) <- makeReg dontCare
    spillFrom4 :: Reg (Bit 1) <- makeReg dontCare

    -- Register to spill
    spillReg4 :: Reg (Bit 5) <- makeReg dontCare

    -- Spill successful?
    spillSuccess6 :: Reg (Bit 1) <- makeDReg false

    -- Vector register mask (for dynamic spilling)
    vecMask2b :: Reg (Bit 32) <- makeReg dontCare

    -- Maintain psuedo rolling average for register use
    regUsage :: Vec 32 (Reg (Bit SIMTRegCountBits)) <- V.replicateM (makeReg 0)

    -- Mask of registers to prioritise for spilling
    spillMaskPref :: Reg (Bit 32) <- makeReg dontCare

    -- Previous register spilled
    spillMaskPrev :: Reg (Bit 32) <- makeReg 0

    -- Tags of affine-scalarisable instructions
    let affineTags = catMaybes [ c.scalarUnitAffineAdd
                               , c.scalarUnitAffineCMove
                               , c.scalarUnitAffineCIncOffset ]

    -- Pipeline Initialisation
    -- =======================

    -- Register to trigger pipeline initialisation at some PC
    startReg :: Reg (Option (Bit 32)) <- makeReg none

    -- Warp id counter, to initialise PC of each thread
    warpIdCounter :: Reg (Bit SIMTLogWarps) <- makeReg 0

    -- Has initialisation completed?
    initComplete :: Reg (Bit 1) <- makeReg false

    always do
      let start = startReg.val
      -- When start register is valid, perform initialisation
      when (start.valid .&. kernelRespQueue.notFull) do
        -- Write PC to each thread of warp
        let initState =
              SIMTThreadState {
                simtPC = start.val
              , simtNestLevel = 0
              , simtRetry = false
              }

        -- Initialise per-warp state
        let initPCC = almightyCapMemVal -- TODO: constrain, or take as param
        sequence_
          [ do stateMem.store (warpIdCounter.val) initState
               if enableCHERI
                 then pccMem.store (warpIdCounter.val) (upper initPCC)
                 else return ()
          | (stateMem, pccMem) <- zip stateMemsA pccMemsA ]

        -- Intialise PCC per warp
        -- TODO: constrain, or take as param
        pccSharedA.store warpIdCounter.val almightyCapPipeVal

        when (warpIdCounter.val .==. 0) do
          -- Register file initialisation
          regFile.init
          capRegFile.init

          -- Reset various state
          excGlobal <== false
          sequence_ [e <== false | e <- excLocals]
          setCount barrierCount 0
          sequence_ [r <== false | r <- barrierBits]
          sequence_ [r <== true | r <- warpQueue]

        -- Finish initialisation and activate pipeline
        if warpIdCounter.val .==. ones
          then do
            startReg <== none
            initComplete <== true
            warpIdCounter <== 0
          else
            warpIdCounter <== warpIdCounter.val + 1

    always do
      let initDone = andList
            [ initComplete.val
            , inv regFile.initInProgress
            , inv capRegFile.initInProgress ]
      when initDone do
        initComplete <== false
        -- Start pipeline
        pipelineActive <== true
        -- Reset counters
        cycleCount <== 0
        instrCount <== 0
        scalarisableInstrCount <== 0
        retryCount <== 0
        suspCount <== 0
        scalarSuspCount <== 0
        scalarAbortCount <== 0
        dramAccessCount <== 0
        sbLoadHitCount <== 0
        sbLoadMissCount <== 0
        sbCapLoadHitCount <== 0
        sbCapLoadMissCount <== 0

    -- Stat counters
    -- =============

    when c.enableStatCounters do
      always do
        when (pipelineActive.val) do
          -- Increment cycle count
          cycleCount <== cycleCount.val + 1

          -- Increment instruction count
          let instrIncs :: [Bit 32] =
                map zeroExtend (map (.val) incInstrCountRegs)
          let instrInc = tree1 (\a b -> reg 0 (a+b)) instrIncs
          let scalarInstrInc =
                incScalarInstrCount.val ? (SIMTLanes, 0)
          if c.useScalarUnit
            then do
              instrCount <== instrCount.val + instrInc + scalarInstrInc
              scalarisableInstrCount <==
                scalarisableInstrCount.val + scalarInstrInc
            else do
              instrCount <== instrCount.val + instrInc
              scalarisableInstrCount <== scalarisableInstrCount.val +
                (if delay false instrScalarisable5.val then instrInc else 0)

          -- Pipeline bubbles
          when incRetryCount.val do retryCount <== retryCount.val + 1
          when incSuspCount.val do suspCount <== suspCount.val + 1
          when c.useScalarUnit do
            when incScalarSuspCount.val do
              scalarSuspCount <== scalarSuspCount.val + 1
            when incScalarAbortCount.val do
              scalarAbortCount <== scalarAbortCount.val + 1

          -- DRAM accesses
          dramAccessCount <==
            dramAccessCount.val +
               zeroExtend inputs.simtDRAMStatSigs.dramLoadSig +
                 zeroExtend inputs.simtDRAMStatSigs.dramStoreSig

          -- Store buffer hit rate
          when inputs.simtCoalStats.incLoadHit do
            if inputs.simtCoalStats.isCapMetaAccess
              then sbCapLoadHitCount <== sbCapLoadHitCount.val + 1
              else sbLoadHitCount <== sbLoadHitCount.val + 1

          when inputs.simtCoalStats.incLoadMiss do
            if inputs.simtCoalStats.isCapMetaAccess
              then sbCapLoadMissCount <== sbCapLoadMissCount.val + 1
              else sbLoadMissCount <== sbLoadMissCount.val + 1

    -- ===============
    -- Vector Pipeline
    -- ===============

    -- Stage 0: Warp Scheduling
    -- ========================

    -- Scheduler history
    schedHistory :: Reg (Bit SIMTWarps) <- makeReg 0

    -- Which warps contain at least one suspended thread?
    warpSuspMask :: [Reg (Bit 1)] <- replicateM SIMTWarps (makeReg false)

    -- Warp chosen by scheduler
    chosenWarp :: Reg (Bit SIMTWarps) <- makeReg dontCare

    -- For dynamic register spilling
    --------------------------------
    -- Regsiter spill mode
    regSpillMode <- makeReg false

    -- Spill from int reg file (otherwise spill from cap reg file)
    regSpillModeIntOrCap <- makeReg 0

    -- Warps currently in vector pipeline spilling a register
    spillingWarps :: [Reg (Bit 1)] <- replicateM SIMTWarps (makeReg false)

    -- Scheduler history for warps that are spilling a register
    schedHistorySpill :: Reg (Bit SIMTWarps) <- makeReg 0

    -- Warp chosen by scheduler to spill a register
    chosenWarpSpill :: Reg (Bit SIMTWarps) <- makeReg dontCare
    --------------------------------

    -- Continous monitoring/buffering of signals needed by scheduler
    always do
      -- Calculate which warps contain a suspended thread
      -- (delayed by one cycle)
      sequence_
        [ b <== orList (map (.val) bs)
        | (b, bs) <- zip warpSuspMask (transpose suspBits) ]

      -- Enable register spill mode when required
      when enableSpill do
        if enableCHERI && c.useSharedVectorScratchpad
          then do
            let totalRegs = regFile.numVecRegs + capRegFile.numVecRegs
            let needSpill = SIMTRegFileSize - totalRegs .<. SIMTWarps
            let doCapSpill = if enableCapSpill
                               then capRegFile.numVecRegs .>=.
                                      SIMTSharedVecSpadCapThreshold
                               else false
            regSpillMode <== if enableRegSpill || enableCapSpill
                               then needSpill else false
            regSpillModeIntOrCap <== if doCapSpill then 1 else 0
          else do
            let needSpill :: forall s n. SIMTRegFile s n -> Bit 1
                needSpill rf = rf.numVecRegsUnused .<. SIMTWarps
            let needRegSpill =
                  if enableRegSpill then needSpill regFile else false
            let needCapSpill =
                  if enableCapSpill then needSpill capRegFile else false
            regSpillMode <== needRegSpill .||. needCapSpill
            regSpillModeIntOrCap <== if needRegSpill then 0 else 1

    -- Scheduler: 1st substage
    always do
      -- Bit mask of available warps
      let avail :: Bit SIMTWarps = fromBitList
            [ w.val .&&. inv s.val .&&.
                (if enableSpill then inv e.val else true)
            | (w, s, e) <- zip3 warpQueue warpSuspMask spillingWarps ]

      -- Fair scheduler
      let (newSchedHistory, chosen) = fairScheduler (schedHistory.val, avail)
      chosenWarp <== chosen

      -- Dynamic register spilling: bit mask of available warps
      let spillAvail :: Bit SIMTWarps = fromBitList
            [ orList [w.val, b.val] .&&. inv e.val .&&. inv susp.val
            | (w, b, e, susp) <-
                zip4 warpQueue barrierBits
                     spillingWarps warpSuspMask ]

      -- Dynamic register spilling: fair scheduler
      let (newSchedHistorySpill, chosenSpill) =
            fairScheduler (schedHistorySpill.val, spillAvail)
      when enableSpill do chosenWarpSpill <== chosenSpill

      -- Trigger stage 1
      when pipelineActive.val do
        -- Do we need to spill registers for dynamic register spilling?
        let spillMode = if enableSpill then regSpillMode.val else false
        -- Select warp for register spill, or normal operation?
        if spillMode
          then do
            when enableSpill do
              when (spillAvail .!=. 0) do
                sequence_
                  [ when c do r <== true
                  | (r, c) <- zip spillingWarps (toBitList chosenSpill) ]
                schedHistorySpill <== newSchedHistorySpill
                spill0 <== true
                spillFrom0 <== regSpillModeIntOrCap.val
                go0 <== true
          else do
            sequence_
              [ when c do r <== false
              | (r, c) <- zip warpQueue (toBitList chosen) ]
            when (avail .!=. 0) do
              schedHistory <== newSchedHistory
              go0 <== true

    -- Scheduler: 2nd substage
    always do
      when go0.val do
        let warp =
              if enableSpill
                then spill0.val ? (chosenWarpSpill.val, chosenWarp.val)
                else chosenWarp.val
        let warpId = binaryEncode warp

        -- Load state for next warp on each lane
        forM_ stateMemsA \stateMem -> do
          stateMem.load warpId

        -- Load PCC for next warp on each lane
        if enableCHERI && not c.useSharedPCC
          then do
            forM_ pccMemsA \pccMem -> do
              pccMem.load warpId
          else return ()

        -- Buffer warp id for stage 1
        warpId1 <== warpId

        -- Trigger stage 1
        go1 <== true
        spill1 <== spill0.val
        spillFrom1 <== spillFrom0.val

    -- Stage 1: Active Thread Selection
    -- ================================

    -- For timing, we split this stage over several cycles
    let stage1Substages = 2

    -- Active threads are those with the max nesting level
    -- On a tie, favour instructions undergoing a retry
    let maxOf a@(a_nest, a_retry, _)
              b@(b_nest, b_retry, _) =
          if (a_nest # a_retry) .>. (b_nest # b_retry) then a else b
    let (_, _, leaderIdx) =
          pipelinedTree1 (stage1Substages-1) maxOf
            [ ( mem.out.simtNestLevel
              , mem.out.simtRetry
              , fromInteger i :: Bit SIMTLogLanes )
            | (mem, i) <- zip stateMemsA [0..] ]

    -- Wait for leader index to be computed
    let states2_tmp =
          [iterateN (stage1Substages-1) buffer (mem.out) | mem <- stateMemsA]
    let pccs2_tmp =
          [iterateN (stage1Substages-1) buffer (mem.out) | mem <- pccMemsA]
  
    -- State and PCC of leader
    let state2 = buffer (states2_tmp ! leaderIdx)
    let pcc2   = buffer (pccs2_tmp ! leaderIdx)

    -- Stat and PCC of all threads
    let stateMemOuts2 = map buffer states2_tmp
    let pccs2 = map buffer pccs2_tmp

    -- Trigger stage 2
    let warpId2 = iterateN stage1Substages buffer warpId1.val
    let spill2 = iterateN stage1Substages (delay 0) spill1.val
    let spillFrom2 = iterateN stage1Substages (delay 0) spillFrom1.val
    let go2 = iterateN stage1Substages (delay 0) go1.val

    -- Stage 2: Instruction Fetch
    -- ==========================

    always do
      -- Compute active thread mask
      let activeList =
            [ state2 === s .&&.
                if enableCHERI && not (c.useSharedPCC)
                  then pcc2 .==. pcc
                  else true
            | (s, pcc) <- zip stateMemOuts2 pccs2]
      let activeMask :: Bit SIMTLanes = fromBitList activeList
      if enableSpill
        then activeMask2b <== spill2 ? (ones, activeMask)
        else activeMask2b <== activeMask

      -- Assert that at least one thread in the warp must be active
      when go2 do
        dynamicAssert (orList activeList)
          "SIMT pipeline error: no active threads in warp"

      -- Issue load to instruction memory
      let pc = state2.simtPC
      instrMemA.load (toInstrAddr pc)

      -- Load per-warp PCC
      when c.useSharedPCC do
        pccSharedA.load warpId2

      -- Get vector register mask for current warp
      when enableSpill do
        let chooseRF i c =
              case (enableRegSpill, enableCapSpill) of
                (False, False) -> dontCare
                (True , False) -> i
                (False, True)  -> c
                (True , True)  -> spillFrom2 ? (c, i)
        let vecMasks = V.zipWith chooseRF
                         regFile.getVecMasks capRegFile.getVecMasks
        let vecMask = vecMasks ! warpId2
        vecMask2b <== vecMask
        when c.useLRUSpill do
          let subset xs = [x | (x, i) <- zip xs [0..], i `mod` 2 == 1]
          let usage = zipWith (\v c -> if v then c.val else ones)
                        (toBitList vecMask) (toList regUsage)
          let min a b = if a .<. b then a else b
          let regUsageMin = tree1 min (subset usage)
          spillMaskPref <==
            pack (V.map (\c -> c.val .<=. regUsageMin) regUsage)
        when c.useRRSpill do
          spillMaskPref <== reverseBits (reverseBits spillMaskPrev.val - 1)

    -- Second instruction fetch stage
    ---------------------------------

    -- Only used when dynamic reg spilling enabled
    -- Used to determine which register to spill

    -- Outputs from second fetch stage
    fetchA3Reg :: Reg RegId <- makeReg dontCare
    spillFail3Reg :: Reg (Bit 1) <- makeReg dontCare
    spillA3Reg :: Reg RegId <- makeReg dontCare

    always do
      when enableSpill do
        let srcRegA = srcA instrMemA.out
        let srcRegB = srcB instrMemA.out
        let dstReg = dst instrMemA.out
        let spillMaskA0 = vecMask2b.val .&.
                            inv (binaryDecode srcRegA
                                   .|. binaryDecode srcRegB
                                   .|. binaryDecode dstReg)
        let spillMaskA1 = spillMaskA0 .&. spillMaskPref.val
        let spillMaskA =
              if c.useLRUSpill || c.useRRSpill
                then (spillMaskA1 .==. 0) ? (spillMaskA0, spillMaskA1)
                else spillMaskA0
        let firstSpillMaskA = firstHot spillMaskA
        spillMaskPrev <== firstSpillMaskA
        let spillA = binaryEncode firstSpillMaskA
        spillA3Reg <== spillA
        let spill2b = delay false spill2
        fetchA3Reg <== spill2b ? (spillA, srcRegA)
        spillFail3Reg <== if enableSpill
          then (spill2b .&&. spillMaskA .==. 0) else false

    -- State for stage 3
    let stage2Delay :: forall a. Bits a => a -> a
        stage2Delay x = if enableSpill then delay zero (delay zero x)
                                       else delay zero x
    let warpId3 = stage2Delay warpId2
    let state3 = stage2Delay state2
    let pcc3 = stage2Delay $ decodeCapPipe $
          if c.useSharedPCC
            then (setAddr pccSharedA.out state2.simtPC).value
            else fromMem $ unpack (pcc2 # state2.simtPC)
    let spill3 = stage2Delay spill2
    let spillFrom3 = stage2Delay spillFrom2
    let spillFail3 = if enableSpill then spillFail3Reg.val else false
    let fetchA3 = if enableSpill then fetchA3Reg.val
                                 else srcA instrMemA.out
    let instr3 = if enableSpill then old instrMemA.out else instrMemA.out
    let activeMask3 =
          if enableSpill then old activeMask2b.val else activeMask2b.val
    let (tagMap3, _) = matchMap False (c.decodeStage) instr3
    let usesCapMetaData3 = 
          if enableCHERI && c.useSharedVectorScratchpad
            then orList
                   [ Map.findWithDefault false tag tagMap3
                   | tag <- c.usesCap ]
            else true
    let go3 = stage2Delay go2

    -- Stage 3: Operand Fetch
    -- ======================

    let pcc4 = delay dontCare pcc3

    always do
      when (go3 .&&. inv capRegFile.stall) do

        -- Fetch operands from register file
        regFile.loadA (warpId3, fetchA3)
        regFile.loadB (warpId3, srcB instr3)

        -- Fetch capability meta-data from register file
        when usesCapMetaData3 do
          capRegFile.loadA (warpId3, fetchA3)
          capRegFile.loadB (warpId3, srcB instr3)

      -- Load eviction status of destination register
      when enableSpill do
        regFile.loadEvictedStatus (warpId3, dst instr3)
        capRegFile.loadEvictedStatus (warpId3, dst instr3)

      -- Is any thread in warp suspended?
      -- (In future, consider only suspension bits of active threads)
      let isSusp3 = orList [map (.val) regs ! warpId3 | regs <- suspBits]
      isSusp4 <== isSusp3 .||. spillFail3

      -- Check PCC
      case c.checkPCCFunc of
        -- CHERI disabled; no check required
        Nothing -> return ()
        -- Check PCC
        Just checkPCC -> do
          let table = checkPCC pcc3
          let exception = orList [cond | (cond, _) <- table]
          when go3 do
            when exception do
              head excLocals <== true
              let trapCode = priorityIf table (excCapCode 0)
              display "SIMT pipeline: PCC exception: code=" trapCode

      -- Handle reg file stall
      when (enableCHERI && enableSpill && c.useSharedVectorScratchpad) do
        when (go3 .&&. capRegFile.stall) do
          if spill3
            then (spillingWarps!warpId3) <== false
            else (warpQueue!warpId3) <== true

      -- Trigger stage 4
      warpId4 <== warpId3
      activeMask4 <== activeMask3
      instr4 <== instr3
      state4 <== state3
      spill4 <== spill3 .&&. inv capRegFile.stall
      spillFrom4 <== spillFrom3
      spillReg4 <== spillA3Reg.val
      go4 <== go3 .&&. inv capRegFile.stall

    -- Stage 4: Operand Latch
    -- ======================

    -- Delay given signal by register file load latency
    let loadDelay :: Bits a => a -> a
        loadDelay inp = iterateN (loadLatency - 1) (delay zero) inp

    -- Extra latch when shared vector spad in use
    let extra :: Bits a => a -> a
        extra inp = if enableCHERI && c.useSharedVectorScratchpad
                      then delay zero inp else inp

    -- Decode instruction
    let delayedInstr4 = loadDelay instr4.val
    let (tagMap4, fieldMap4) = matchMap False (c.decodeStage) delayedInstr4

    -- Stage 5 register operands
    let rfAOut = V.map (slice @31 @0) regFile.outA
    let rfBOut = V.map (slice @31 @0) regFile.outB
    let vecRegA5 = old $ extra rfAOut
    let vecRegB5 = old $ extra rfBOut

    -- Stage 5 capability register operands
    let getCapReg intReg capReg =
          old $ decodeCapMem (extra (capReg # intReg))
    let vecCapRegA5 = V.zipWith getCapReg rfAOut capRegFile.outA
    let vecCapRegB5 = V.zipWith getCapReg rfBOut capRegFile.outB
    let vecRawCapMetaRegA5 = V.map (old . extra) capRegFile.outA

    -- Determine if field is available in current instruction
    let isFieldInUse fld fldMap =
          case Map.lookup fld fldMap of
            Nothing -> false
            Just opt -> opt.valid
    let usesA = isFieldInUse "rs1" fieldMap4
    let usesB = isFieldInUse "rs2" fieldMap4
    let usesDest = isFieldInUse "rd" fieldMap4

    -- Register unspilling (fetching)
    let needsDest4 = usesDest .&&. loadDelay (activeMask4.val .!=. ones)
    let unspill4 = if not enableSpill then false else orList [
            usesA .&&. regFile.evictedA
          , usesA .&&. capRegFile.evictedA
          , usesB .&&. regFile.evictedB
          , usesB .&&. capRegFile.evictedB
          , needsDest4 .&&. regFile.evictedStatus
          , needsDest4 .&&. capRegFile.evictedStatus ]
    let unspillTo4 = orList [
            usesA .&&. capRegFile.evictedA
          , usesB .&&. capRegFile.evictedB
          , needsDest4 .&&. capRegFile.evictedStatus ]
    let srcA4 = srcA delayedInstr4
    let srcB4 = srcB delayedInstr4
    let dest4 = dst delayedInstr4
    let unspillReg4 = if unspillTo4
          then (usesA .&&. capRegFile.evictedA) ? (srcA4,
                  (usesB .&&. capRegFile.evictedB) ? (srcB4, dest4))
          else (usesA .&&. regFile.evictedA) ? (srcA4,
                  (usesB .&&. regFile.evictedB) ? (srcB4, dest4))
    let unspillTo5 = old $ extra unspillTo4
    let unspillReg5 = old $ extra unspillReg4
    let unspill4_5 = unspill4 .&&. loadDelay (go4.val .&&. inv spill4.val)
    let unspill5 = delay false $ extra unspill4_5

    -- Stage 5 scalarised operands
    let trunc32ScalarVal x = ScalarVal { val = slice @31 @0 x.val
                                       , stride = x.stride }
    let scalarisedOperandB5 =
          ScalarisedOperand {
            scalarisedVal = old $ extra (fmap trunc32ScalarVal regFile.scalarB)
          , scalarisedCapVal = old $ extra capRegFile.scalarB
          }

    -- Stage 5 register B or immediate
    let getRegBorImm reg = old $ extra $
          if Map.member "imm" fieldMap4 && Map.member "rs2" fieldMap4
            then let imm = getBitField fieldMap4 "imm"
                     rs2 = getBitField fieldMap4 "rs2" :: Option RegId
                     imm_r = imm.valid ? (imm.val, reg)
                     r_imm = rs2.valid ? (reg, imm.val)
                 in  (imm_r, r_imm)
            else (reg, reg)
    let vecRegBorImm5 = V.map getRegBorImm rfBOut

    -- Propagate signals to stage 5
    let pcc5 = old $ extra (loadDelay pcc4)
    let isSusp5 = old $ extra (loadDelay isSusp4.val)
    let warpId5 = old $ extra (loadDelay warpId4.val)
    let activeMask5 = old $ extra (if unspill4_5 then ones
                                     else loadDelay activeMask4.val)
    let instr5 = old $ extra (loadDelay instr4.val)
    let state5 = old $ extra (loadDelay state4.val)
    let spill5 = delay false $ extra (loadDelay spill4.val)
    let spillFrom5 = old $ extra (loadDelay spillFrom4.val)
    let spillReg5 = old $ extra (loadDelay spillReg4.val)
    let go5 = delay false $ extra (loadDelay (go4.val .&&. inv spill4.val)
                                     .&&. inv unspill4)

    -- Buffer the decode tables
    let tagMap5 = Map.map (old . extra) tagMap4

    -- Determine if this instruction is scalarisable
    when c.useRegFileScalarisation do
      always do
        let isAffineA = regFile.scalarA.valid .&&.
              if enableCHERI then capRegFile.scalarA.valid else true
        let isAffineB = regFile.scalarB.valid .&&.
              if enableCHERI then capRegFile.scalarB.valid else true
        let isUniformA = isAffineA .&&. regFile.scalarA.val.stride .==. 0
        let isUniformB = isAffineB .&&. regFile.scalarB.val.stride .==. 0
        let allUniform = (usesA .==>. isUniformA) .&&. (usesB .==>. isUniformB)
        let allAffine = (usesA .==>. isAffineA) .&&. (usesB .==>. isAffineB)
        let oneUniform = allAffine .&&.
              ((usesA .==>. isUniformA) .||. (usesB .==>. isUniformB))
        let affineTagActive = orList
              [ Map.findWithDefault false affineTag tagMap4
              | affineTag <- affineTags ]
        let areOperandsScalar = if affineTagActive then oneUniform
                                                   else allUniform
        let isOpcodeScalarisable = orList
              [ Map.findWithDefault false op tagMap4
              | op <- c.scalarUnitAllowList ]
        instrScalarisable5 <== extra (andList
          [ loadDelay (activeMask4.val .==. ones)
          , isOpcodeScalarisable
          , areOperandsScalar
          ])

    -- Stages 5: Execute
    -- =================

    -- For each lane, is PC being explicitly modified by instruction?
    pcChangeRegs6 :: [Reg (Bit 1)] <- replicateM SIMTLanes (makeDReg false)

    -- Insert warp id back into warp queue, except on warp command
    always do
      -- Lookup scalar prediction table
      -- (NOTE: consider looking up lane 0's pcNext here)
      scalarTableA.load (toInstrAddr (state5.simtPC + 4))

      when go5 do
        -- Maintain approximate rolling average of register usage
        when (enableSpill && c.useLRUSpill) do
          let anyFull = orList (map (.==. ones) (map (.val) (toList regUsage)))
          let incA i = delay false usesA .&&. srcA instr5 .==. fromInteger i
          let incB i = delay false usesB .&&. srcB instr5 .==. fromInteger i
          when (warpId5 .==. 0) do
            sequence_ [ if anyFull
                          then r <== false # upper r.val
                          else when (incA i .||. incB i) do r <== r.val + 1
                      | (r, i) <- zip (drop 1 $ toList regUsage) [1..31] ]

        -- Update stat counters
        when isSusp5 do incSuspCount <== true

        -- Update scalar prediction table
        when (inv isSusp5) do
          scalarTableA.store (toInstrAddr state5.simtPC)
                             instrScalarisable5.val

        -- Check that instruction is recognised
        let known = orList [valid | (_, valid) <- Map.toList tagMap5]
        when (inv known) do
          display "Instruction not recognised @ PC="
            (formatHex 8 state5.simtPC)

        -- Reschedule warp if any thread suspended, or the instruction
        -- is not a warp command and an exception has not occurred
        if isSusp5 .||.
              (inv inputs.simtWarpCmdWire.active .&&. inv excGlobal.val)
          then do
            rescheduleWarp6 <== true
          else do
            -- Instruction is warp command or exception has occurred.
            -- Warp commands assume that warp has converged
            dynamicAssert (inv excGlobal.val .==>. activeMask5 .==. ones)
              "SIMT pipeline: warp command issued by diverged warp"
            -- Handle command
            if inputs.simtWarpCmdWire.val.warpCmd_isTerminate .||.
                 excGlobal.val
              then do
                completedWarps <== completedWarps.val + 1
                -- Track kernel success
                let success = kernelSuccess.val .&.
                      inputs.simtWarpCmdWire.val.warpCmd_termCode
                -- Determined completed warps
                let completed = completedWarps.val +
                      (excGlobal.val ? (barrierCount.getCount, 0))
                -- Have all warps have terminated?
                if completed .==. ones
                  then do
                    -- Issue kernel response to CPU
                    dynamicAssert (kernelRespQueue.notFull)
                      "SIMT pipeline: can't issue kernel response"
                    let code = excGlobal.val ? (simtExit_Exception,
                          success ? (simtExit_Success, simtExit_Failure))
                    enq kernelRespQueue (zeroExtend code)
                    -- Re-enter initial state
                    pipelineActive <== false
                    kernelSuccess <== true
                  else do
                    -- Update kernel success
                    kernelSuccess <== success
              else do
                -- Enter barrier
                barrierBits!warpId5 <== true
                incrBy barrierCount 1

      -- Promote local exception to global exception
      when (orList $ map (.val) excLocals) do
        excGlobal <== true
        -- Track exception PC
        excPC <== old state5.simtPC

    -- Is it a SIMT convergence instruction?
    let isSIMTPush = Map.findWithDefault false c.simtPushTag tagMap5
    let isSIMTPop = Map.findWithDefault false c.simtPopTag tagMap5

    -- Per-lane result wires
    resultWires :: [Wire (Bit 32)] <-
      replicateM SIMTLanes (makeWire dontCare)
    resultCapWires :: [Wire CapMemMeta] <-
      replicateM SIMTLanes (makeWire dontCare)

    -- Next PC
    let pcPlusFour = state5.simtPC + 4

    -- Vector lane definition
    let makeLane makeExecStage threadActive suspMask regA regB regBorImm
                 capRegA capRegB stateMem incInstrCount pccMem
                 excLocal laneId resultWire resultCapWire pcChange = do

          -- Per lane interfacing
          pcNextWire :: Wire (Bit 32) <- makeWire pcPlusFour
          pccNextWire :: Wire CapPipe <- makeWire dontCare
          retryWire  :: Wire (Bit 1) <- makeWire false
          suspWire   :: Wire (Bit 1) <- makeWire false

          -- Is destination register non-zero?
          let destNonZero = dst instr5 .!=. 0

          -- Instantiate execute stage
          execStage <- makeExecStage
            State {
              instr = instr5
            , opA = regA
            , opB = regB
            , immOrOpB = fst regBorImm
            , opBorImm = snd regBorImm
            , opAIndex = srcA instr5
            , opBIndex = srcB instr5
            , resultIndex = dst instr5
            , pc = ReadWrite (state5.simtPC) \pcNew -> do
                pcNextWire <== pcNew
            , result = WriteOnly \writeVal ->
                         when destNonZero do
                           resultWire <== writeVal
                           if enableCHERI
                             then resultCapWire <== nullCapMemMetaVal
                             else return ()
            , suspend = suspWire <== true
            , retry = retryWire <== true
            , opcode = packTagMap tagMap5
            , trap = \code -> do
                excLocal <== true
                display "SIMT exception occurred: " code
                        " pc=0x" (formatHex 8 (state5.simtPC))
            -- CHERI support
            , capA = capRegA
            , capB = capRegB
            , pcc = pcc5
            , pccNew = WriteOnly \pccNew -> do
                pcNextWire <== getAddr pccNew
                pccNextWire <== pccNew
            , resultCap = WriteOnly \cap ->
                            when destNonZero do
                              let capMem = pack (toMem cap)
                              resultWire <== lower capMem
                              resultCapWire <== upper capMem
            }

          always do
            -- Did PC change?
            pcChange <== pcNextWire.active .||. retryWire.val

            -- Update stat counters
            when retryWire.val do incRetryCount <== true

            -- Execute stage
            when (go5 .&&. threadActive .&&.
                    inv isSusp5 .&&. inv excGlobal.val) do
              -- Trigger execute stage
              execStage.execute

              -- Update thread state
              let nestInc = zeroExtendCast isSIMTPush
              let nestDec = zeroExtendCast isSIMTPop
              let nestLevel = state5.simtNestLevel
              dynamicAssert (isSIMTPop .==>. nestLevel .!=. 0)
                  "SIMT pipeliene: SIMT nest level underflow"
              dynamicAssert (isSIMTPush .==>. nestLevel .!=. ones)
                  "SIMT pipeliene: SIMT nest level overflow"
              stateMem.store warpId5
                SIMTThreadState {
                    -- Only update PC if not retrying
                    simtPC = retryWire.val ?
                      (state5.simtPC, pcNextWire.val)
                  , simtNestLevel = (nestLevel + nestInc) - nestDec
                  , simtRetry = retryWire.val
                  }
              when (delay false pccNextWire.active) do
                pccMem.store (old warpId5)
                             (old $ upper $ pack $ toMem pccNextWire.val)

              -- Increment instruction count
              when (inv retryWire.val) do
                incInstrCount <== true

              -- Update suspension bits
              -- To allow latency in writeback, mark any thread
              -- writing a result as suspended
              when (suspWire.val .||. resultWire.active) do
                suspMask!warpId5 <== true

    -- Create vector lanes
    sequence $ getZipList $
      makeLane <$> ZipList (c.executeStage)
               <*> ZipList (toBitList activeMask5)
               <*> ZipList suspBits
               <*> ZipList (toList vecRegA5)
               <*> ZipList (toList vecRegB5)
               <*> ZipList (toList vecRegBorImm5)
               <*> ZipList (toList vecCapRegA5)
               <*> ZipList (toList vecCapRegB5)
               <*> ZipList stateMemsA
               <*> ZipList incInstrCountRegs
               <*> ZipList pccMemsA
               <*> ZipList excLocals
               <*> ZipList [0..]
               <*> ZipList resultWires
               <*> ZipList resultCapWires
               <*> ZipList pcChangeRegs6

    -- Dynamic register spilling
    when enableSpill do
      always do
        when spill5 do
          if isSusp5
            then do
              -- Update stat counters
              incSuspCount <== true
            else do
              -- Assumption: all lanes can request if first can
              let canPutMemReq = (head $ toList inputs.simtMemReqs).canPut
              -- Address of vector reg in DRAM
              let addrLow :: Bit (SIMTLogLanes+2) = 0
              let addr :: Bit 32 =
                      fromInteger c.regSpillBaseAddr
                    + zeroExtend (spillFrom5 # warpId5 # spillReg5 # addrLow)
              -- Submit mem request for each lane
              sequence_
                [ when memReqs.canPut do
                    memReqs.put
                      MemReq {
                        memReqAccessWidth = 2
                      , memReqOp = memStoreOp
                      , memReqAMOInfo = dontCare
                      , memReqAddr = addr + fromInteger (4*laneId)
                      , memReqData = spillFrom5 ? (lower capVal, val)
                      , memReqDataTagBit = spillFrom5 ? (upper capVal, 0)
                      , memReqDataTagBitMask = 0
                      , memReqIsUnsigned = dontCare
                      , memReqIsFinal = true
                      }
                | (memReqs, laneId, val, capVal) <-
                    zip4 (toList inputs.simtMemReqs) [0..]
                         (toList vecRegA5) (toList vecRawCapMetaRegA5) ]
              when canPutMemReq do
                spillSuccess6 <== true

    -- Register unspilling (fetching)
    when enableSpill do
      always do
        when unspill5 do
          rescheduleWarp6 <== true
          if isSusp5
            then do
              -- Update stat counters
              incSuspCount <== true
            else do
              -- Assumption: all lanes can request if first can
              let canPutMemReq = (head $ toList inputs.simtMemReqs).canPut
              -- Address of vector reg in DRAM
              let addrLow :: Bit (SIMTLogLanes+2) = 0
              let addr :: Bit 32 =
                      fromInteger c.regSpillBaseAddr
                    + zeroExtend (unspillTo5 # warpId5 # unspillReg5 # addrLow)
              -- Submit mem request for each lane
              sequence_
                [ when memReqs.canPut do
                    memReqs.put
                      MemReq {
                        memReqAccessWidth = 2
                      , memReqOp = memLoadOp
                      , memReqAMOInfo = dontCare
                      , memReqAddr = addr + fromInteger (4*laneId)
                      , memReqData = dontCare
                      , memReqDataTagBit = dontCare
                      , memReqDataTagBitMask = 1
                      , memReqIsUnsigned = 0
                      , memReqIsFinal = true
                      }
                | (memReqs, laneId) <-
                    zip (toList inputs.simtMemReqs) [0..] ]
              -- Suspend the warp
              when canPutMemReq do
                sequence_ [(regs ! warpId5) <== true | regs <- suspBits]

    -- Stage 6: Writeback
    -- ==================

    always do
      let warpId6 = old warpId5
      let isSusp6 = delay false isSusp5
      -- Process data from Execute stage
      let executeIdx = (warpId6, old (dst instr5))
      let executeVec :: Bits t => [Wire t] -> Vec SIMTLanes (Option t)
          executeVec resWires = fromList
            [ Option (inv excLocal.val .&&. delay false resultWire.active)
                     (old resultWire.val)
            | (excLocal, resultWire) <- zip excLocals resWires ]
      -- Process data from resumption queue
      let (resumeInfo, resumeVec) = inputs.simtResumeReqs.peek
      let resumeIdx = (resumeInfo.warpId, resumeInfo.destReg)
      let resumeVecInt = fromList
            [ Option (req.valid .&&. resumeInfo.destReg .!=. 0 .&&.
                       inv excGlobal.val)
                     req.val.resumeReqData
            | req <- toList resumeVec ]
      let resumeUnspill = resumeInfo.regFileId .!=. regFileIntCap
      let resumeVecCap = fromList
            [ Option (req.valid .&&. resumeInfo.destReg .!=. 0 .&&.
                       inv excGlobal.val)
                     (if req.val.resumeReqCap.valid .||. resumeUnspill
                        then req.val.resumeReqCap.val
                        else nullCapMemMetaVal)
            | req <- toList resumeVec ]
      -- Handle writeback for execute or resumption?
      let handleExecute = delay false $ orList
            [ resultWire.active .||. capResultWire.active
            | (resultWire, capResultWire) <- zip resultWires resultCapWires ]
      -- Select one of the writes
      let writeIdx = handleExecute ? (executeIdx, resumeIdx)
      let writeVec = handleExecute ? (executeVec resultWires, resumeVecInt)
      let writeCapVec = handleExecute ? (executeVec resultCapWires,
                                           resumeVecCap)
      -- Only resumptions need to check the 'canStore' condition to
      -- avoid potential hazards in the RF implementation; normal
      -- writebacks to the same register are too infrequent to cause
      -- a hazard.
      let handleResume = inputs.simtResumeReqs.canPeek .&&.
                           regFile.canStore writeIdx .&&.
                             capRegFile.canStore writeIdx .&&.
                               inv spillSuccess6.val

      -- Write to int reg file?
      let writeInt = handleExecute .||. (handleResume .&&.
                       resumeInfo.regFileId .!=. regFileCapMeta)
      -- Write to cap meta reg file?
      let writeCapMeta = handleExecute .||. (handleResume .&&.
                           resumeInfo.regFileId .!=. regFileInt)
      -- Write to register file(s)
      when writeInt do
        regFile.store writeIdx (V.map (fmap (false #)) writeVec)
      when enableCHERI do
        when writeCapMeta do
          capRegFile.store writeIdx writeCapVec
      -- Handle thread resumption
      when (inv handleExecute .&&. handleResume) do
        inputs.simtResumeReqs.consume

      -- Clear suspension bit after write latency elapses
      let latency = max regFile.storeLatency capRegFile.storeLatency
      let getActiveBit result resume = iterateN latency (delay false)
            (handleExecute ? (result.valid, resume.valid))
      -- Which threads need to be resumed?
      let active = zipWith getActiveBit
            (toList $ executeVec resultWires) (toList resumeVec)
      let warpId = iterateN latency (delay dontCare) writeIdx.fst
      let doClear = iterateN latency (delay false)
            (handleExecute .||. handleResume)
      when doClear do
        sequence_
          [ when valid do suspMask!warpId <== false
          | (valid, suspMask) <- zip active suspBits ]

      -- Reschedule warp
      when rescheduleWarp6.val do
        -- Put warp in scalar or vector queue?
        let putInScalarQueue =
              -- (NOTE: consider relaxing these restrictions)
              if c.useScalarUnit
                then andList
                  [ old activeMask5 .==. ones
                  , scalarTableA.out
                  , inv $ orList $ map (.val) pcChangeRegs6
                  , inv isSusp6
                  , inv (delay false unspill5)
                  ]
                else false
        if putInScalarQueue .&&. inv scalarUnitWarpCount.isFull
          then do
            (scalarQueue!warpId6) <== true
            scalarUnitWarpCount.incrBy 1
          else do
            (warpQueue!warpId6) <== true

      -- Dynamical spill: finish off register spill
      when enableSpill do
        let spill6 = delay false spill5
        let spillFrom6 = delay 0 spillFrom5
        let spillReg6 = delay 0 spillReg5
        when (iterateN latency (delay false) spill6) do
          let warpId = iterateN latency old warpId6
          (spillingWarps!warpId) <== false
        when spillSuccess6.val do
          if spillFrom6
            then capRegFile.evict (warpId6, spillReg6)
            else regFile.evict (warpId6, spillReg6)

    -- ===============
    -- Scalar Pipeline
    -- ===============

    -- Scalar pipeline stage trigger signals
    scalarGo0 :: Reg (Bit 1) <- makeDReg false
    scalarGo1 :: Reg (Bit 1) <- makeDReg false
    scalarGo2 :: Reg (Bit 1) <- makeDReg false
    scalarGo3 :: Reg (Bit 1) <- makeDReg false
    scalarGo4 :: Reg (Bit 1) <- makeDReg false
    scalarGo5 :: Reg (Bit 1) <- makeDReg false

    -- Instruction register, per pipeline stage
    scalarInstr3 :: Reg (Bit 32) <- makeReg dontCare
    scalarInstr4 :: Reg (Bit 32) <- makeReg dontCare
    scalarInstr5 :: Reg (Bit 32) <- makeReg dontCare

    -- Warp id, per pipeline stage
    scalarWarpId1 :: Reg (Bit SIMTLogWarps) <- makeReg dontCare
    scalarWarpId2 :: Reg (Bit SIMTLogWarps) <- makeReg dontCare
    scalarWarpId3 :: Reg (Bit SIMTLogWarps) <- makeReg dontCare
    scalarWarpId4 :: Reg (Bit SIMTLogWarps) <- makeReg dontCare
    scalarWarpId5 :: Reg (Bit SIMTLogWarps) <- makeReg dontCare

    -- Warp state, per pipeline stage
    scalarState2 :: Reg SIMTThreadState <- makeReg dontCare
    scalarState3 :: Reg SIMTThreadState <- makeReg dontCare
    scalarState4 :: Reg SIMTThreadState <- makeReg dontCare
    scalarState5 :: Reg SIMTThreadState <- makeReg dontCare

    -- Warp PCC, per pipeline stage
    scalarPCC2 :: Reg CapPipe <- makeReg dontCare
    scalarPCC3 :: Reg Cap <- makeReg dontCare
    scalarPCC4 :: Reg Cap <- makeReg dontCare

    -- Warp suspension status, per pipeline stage
    scalarIsSusp3 :: Reg (Bit 1) <- makeReg dontCare
    scalarIsSusp4 :: Reg (Bit 1) <- makeReg dontCare
    scalarIsSusp5 :: Reg (Bit 1) <- makeReg dontCare

    -- Instruciton operand registers
    scalarOpA4 :: Reg (ScalarVal 32) <- makeReg dontCare
    scalarOpB4 :: Reg (ScalarVal 32) <- makeReg dontCare
    scalarOpBOrImm4 <- makeReg dontCare
    scalarCapA4 :: Reg Cap <- makeReg dontCare
    scalarCapB4 :: Reg Cap <- makeReg dontCare
    scalarCapMemMetaA4 :: Reg CapMemMeta <- makeReg dontCare

    -- Abort scalar pipeline if instruction turns out not to be scalarisable
    scalarAbort4 :: Reg (Bit 1) <- makeReg dontCare
    scalarAbort5 :: Reg (Bit 1) <- makeReg dontCare

    -- Retry/suspend wires from execute stage
    scalarSuspend5 :: Reg (Bit 1) <- makeDReg false
    scalarRetry5 :: Reg (Bit 1) <- makeDReg false

    when c.useScalarUnit do

      -- Scheduler history
      scalarSchedHistory :: Reg (Bit SIMTWarps) <- makeReg 0

      -- Warp chosen by scheduler
      scalarChosenWarp :: Reg (Bit SIMTLogWarps) <- makeReg dontCare

      -- Stage 0: Warp Scheduling
      -- ========================

      -- Scheduler: 1st substage
      always do
        -- Bit mask of available warps
        let avail :: Bit SIMTWarps =
              fromBitList [ r.val .&&. inv s.val .&&.
                              (if enableSpill then inv e.val else true)
                          | (r, s, e) <- zip3 scalarQueue warpSuspMask
                                              spillingWarps ]

        -- Fair scheduler
        let (newSchedHistory, chosen) =
              fairScheduler (scalarSchedHistory.val, avail)
        scalarChosenWarp <== binaryEncode chosen

        -- Update scalar queue
        sequence_
          [ when c do r <== false
          | (r, c) <- zip scalarQueue (toBitList chosen) ]

        -- Trigger next stage
        when (avail .!=. 0) do
          scalarSchedHistory <== newSchedHistory
          scalarGo0 <== true

      -- Scheduler: 2nd substage
      always do
        let warpId = scalarChosenWarp.val

        -- Lookup warp's PC
        (head stateMemsB).load warpId

        -- Lookup warp's PCC
        if c.useSharedPCC
          then do pccSharedB.load warpId
          else do (head pccMemsB).load warpId

        -- Trigger stage 1
        when scalarGo0.val do
          scalarGo1 <== true
          scalarWarpId1 <== warpId

      -- Stage 1: Instruction Fetch
      -- ==========================

      always do
        let state = (head stateMemsB).out
        let pcc =
              if c.useSharedPCC
                then (setAddr pccSharedB.out state.simtPC).value
                else fromMem $ unpack ((head pccMemsB).out # state.simtPC)

        -- Load next instruction
        instrMemB.load (toInstrAddr state.simtPC)

        -- Trigger next stage
        scalarGo2 <== scalarGo1.val
        scalarWarpId2 <== scalarWarpId1.val
        scalarState2 <== state
        scalarPCC2 <== pcc

      -- Stage 2: Operand Fetch
      -- ======================

      always do
        -- Fetch operands from register file
        regFile.loadScalarC (scalarWarpId2.val, srcA instrMemB.out)
        regFile.loadScalarD (scalarWarpId2.val, srcB instrMemB.out)
        when enableCHERI do
          capRegFile.loadScalarC (scalarWarpId2.val, srcA instrMemB.out)
          capRegFile.loadScalarD (scalarWarpId2.val, srcB instrMemB.out)
        -- Is any thread in warp suspended?
        scalarIsSusp3 <== orList [ map (.val) regs ! scalarWarpId2.val
                                 | regs <- suspBits ]
        -- Trigger next stage
        scalarGo3 <== scalarGo2.val
        scalarWarpId3 <== scalarWarpId2.val
        scalarState3 <== scalarState2.val
        scalarPCC3 <== decodeCapPipe scalarPCC2.val
        scalarInstr3 <== instrMemB.out

      -- Stage 3: Operand Latch
      -- ======================

      -- Decode instruction
      let (scalarTagMap3, scalarFieldMap3) =
            matchMap False c.scalarUnitDecodeStage scalarInstr3.val

      -- Use "imm" field if valid, otherwise use register b
      let bOrImm =
            if Map.member "imm" scalarFieldMap3 &&
               Map.member "rs2" scalarFieldMap3
              then
                let imm = getBitField scalarFieldMap3 "imm"
                    rs2 = getBitField scalarFieldMap3 "rs2" :: Option RegId
                    choice = ( ScalarVal { val = imm.val, stride = 0 }
                             , regFile.scalarD.val )
                    swap (x, y) = (y, x)
                    imm_r = imm.valid ? choice
                    r_imm = rs2.valid ? (swap choice)
                 in (imm_r, r_imm)
              else (regFile.scalarD.val, regFile.scalarD.val)

      always do
        -- Trigger next stage
        scalarGo4 <== scalarGo3.val
        -- Latch operands
        scalarOpA4 <== trunc32ScalarVal regFile.scalarC.val
        scalarOpB4 <== trunc32ScalarVal regFile.scalarD.val
        scalarOpBOrImm4 <== (trunc32ScalarVal $ fst bOrImm,
                             trunc32ScalarVal $ snd bOrImm)
        when enableCHERI do
          scalarCapA4 <==
            decodeCapMem (capRegFile.scalarC.val.val #
                          slice @31 @0 regFile.scalarC.val.val)
          scalarCapB4 <==
            decodeCapMem (capRegFile.scalarD.val.val #
                          slice @31 @0 regFile.scalarD.val.val)
          scalarCapMemMetaA4 <== capRegFile.scalarC.val.val
        scalarWarpId4 <== scalarWarpId3.val
        scalarState4 <== scalarState3.val
        scalarPCC4 <== scalarPCC3.val
        scalarInstr4 <== scalarInstr3.val
        scalarIsSusp4 <== scalarIsSusp3.val
        -- Check PCC
        let pccFail =
              case c.checkPCCFunc of
                -- CHERI disabled; no check required
                Nothing -> false
                -- Check PCC
                Just checkPCC ->
                  let table = checkPCC scalarPCC3.val
                      exception = orList [cond | (cond, _) <- table]
                   in exception .&&. scalarGo3.val
        -- Check that instruction can indeed run on scalar unit
        let usesA = isFieldInUse "rs1" scalarFieldMap3
        let usesB = isFieldInUse "rs2" scalarFieldMap3
        let isAffineA = regFile.scalarC.valid .&&.
              if enableCHERI then capRegFile.scalarC.valid else true
        let isAffineB = regFile.scalarD.valid .&&.
              if enableCHERI then capRegFile.scalarD.valid else true
        let isUniformA = isAffineA .&&. regFile.scalarC.val.stride .==. 0
        let isUniformB = isAffineB .&&. regFile.scalarD.val.stride .==. 0
        let allUniform = (usesA .==>. isUniformA) .&&. (usesB .==>. isUniformB)
        let allAffine = (usesA .==>. isAffineA) .&&. (usesB .==>. isAffineB)
        let oneUniform = allAffine .&&.
              ((usesA .==>. isUniformA) .||. (usesB .==>. isUniformB))
        let affineTagActive = orList
              [ Map.findWithDefault false affineTag scalarTagMap3
              | affineTag <- affineTags ]
        let areOperandsScalar = if affineTagActive then oneUniform
                                                   else allUniform
        let isOpcodeScalarisable = orList
              [ Map.findWithDefault false op scalarTagMap3
              | op <- c.scalarUnitAllowList ]
        scalarAbort4 <== inv areOperandsScalar
                    .||. inv isOpcodeScalarisable
                    .||. pccFail
       
      -- Stage 4: Execute & Suspend
      -- ==========================

      -- Execute stage wires
      let pcPlusFour = scalarState4.val.simtPC + 4
      scalarPCNextWire :: Wire (Bit 32) <- makeWire pcPlusFour
      scalarResultWire :: Wire (Bit 32) <- makeWire dontCare
      scalarResultStrideWire :: Wire (Bit SIMTAffineScalarisationBits) <-
        makeWire 0
      scalarPCCNextWire :: Wire CapPipe <-
        makeWire (setAddrUnsafe scalarPCC4.val.capPipe pcPlusFour)
      scalarResultCapWire :: Wire CapMemMeta <- makeWire dontCare

      let scalarTagMap4 = Map.map old scalarTagMap3

      -- SIMT convergence registers nesting level
      scalarNestLevel5 :: Reg (Bit SIMTLogMaxNestLevel) <- makeReg dontCare
      always do
        -- Compute SIMT nesting level
        let isSIMTPush = Map.findWithDefault false c.simtPushTag scalarTagMap4
        let isSIMTPop = Map.findWithDefault false c.simtPopTag scalarTagMap4
        let nestInc = zeroExtendCast isSIMTPush
        let nestDec = zeroExtendCast isSIMTPop
        scalarNestLevel5 <==
         (scalarState4.val.simtNestLevel + nestInc) - nestDec

      -- Instantiate execute stage
      execStage <- c.scalarUnitExecuteStage
        State {
          instr = scalarInstr4.val
        , opA = scalarOpA4.val.val
        , opB = scalarOpB4.val.val
        , immOrOpB = (fst scalarOpBOrImm4.val).val
        , opBorImm = (snd scalarOpBOrImm4.val).val
        , opAIndex = srcA scalarInstr4.val
        , opBIndex = srcB scalarInstr4.val
        , resultIndex = dst scalarInstr4.val
        , pc = ReadWrite scalarState4.val.simtPC \pcNew -> do
                 scalarPCNextWire <== pcNew
        , result = WriteOnly \x ->
                     when (dst scalarInstr4.val .!=. 0) do
                       scalarResultWire <== x
                       if enableCHERI
                         then scalarResultCapWire <== nullCapMemMetaVal
                         else return ()
        , suspend = do scalarSuspend5 <== true
        , retry = do scalarRetry5 <== true
        , opcode =
            -- Filter out instructions overridden by built-in affine versions
            packTagMap $ foldr (\tag m -> Map.insert tag false m)
                               scalarTagMap4 affineTags
        , trap = \code -> do
            -- TODO: handle trap
            display "Scalar unit trap: code=" code
                    " pc=0x" (formatHex 8 scalarState4.val.simtPC)
        , capA = scalarCapA4.val
        , capB = scalarCapB4.val
        , pcc = scalarPCC4.val
        , pccNew = WriteOnly \pccNew -> do
            scalarPCNextWire <== getAddr pccNew
            scalarPCCNextWire <== pccNew
        , resultCap = WriteOnly \cap ->
            when (dst scalarInstr4.val .!=. 0) do
              let capMem = pack (toMem cap)
              scalarResultWire <== lower capMem
              scalarResultCapWire <== upper capMem
        }

      -- For aborting built-in affine operation
      affineAbortWire <- makeWire false

      always do
        when scalarGo4.val do
          -- Update stats
          if scalarIsSusp4.val
            then do incScalarSuspCount <== true
            else do
              when scalarAbort4.val do
                incScalarAbortCount <== true

          -- Invoke execute stage
          when (inv scalarIsSusp4.val .&&.
                  inv scalarAbort4.val) do
            execStage.execute
            incScalarInstrCount <== true

            -- Check that instruction is recognised
            let known = orList [valid | (_, valid) <- Map.toList scalarTagMap4]
            when (inv known) do
              display "Instruction not recognised @ PC="
                (formatHex 8 scalarState4.val.simtPC)

            let dstNonZero = dst scalarInstr4.val .!=. 0

            -- Built-in affine add
            case c.scalarUnitAffineAdd of
              Nothing -> return ()
              Just addOp -> do
                when (Map.findWithDefault false addOp scalarTagMap4) do
                  when dstNonZero do
                    scalarResultWire <==
                      scalarOpA4.val.val + (fst scalarOpBOrImm4.val).val
                    scalarResultStrideWire <==
                      scalarOpA4.val.stride + (fst scalarOpBOrImm4.val).stride
                    when enableCHERI do
                      scalarResultCapWire <== nullCapMemMetaVal

            -- Built-in affine cmove
            case c.scalarUnitAffineCMove of
              Nothing -> return ()
              Just cmoveOp -> do
                when (Map.findWithDefault false cmoveOp scalarTagMap4) do
                  when dstNonZero do
                    scalarResultWire <== scalarOpA4.val.val
                    scalarResultStrideWire <== scalarOpA4.val.stride
                    when enableCHERI do
                      scalarResultCapWire <== scalarCapMemMetaA4.val

            -- Built-in affine cincoffset
            case c.scalarUnitAffineCIncOffset of
              Nothing -> return ()
              Just cincOp -> do
                when (Map.findWithDefault false cincOp scalarTagMap4) do
                  when dstNonZero do
                    let sum = scalarOpA4.val.val +
                              (fst scalarOpBOrImm4.val).val
                    scalarResultWire <== sum
                    scalarResultStrideWire <==
                      scalarOpA4.val.stride + (fst scalarOpBOrImm4.val).stride
                    when enableCHERI do
                      scalarResultCapWire <== scalarCapMemMetaA4.val
                    -- Abort as result could be out of representable bounds
                    when (zeroExtend sum .>. scalarCapA4.val.capTop) do
                      affineAbortWire <== true

        -- Lookup scalar prediction table
        scalarTableB.load (toInstrAddr scalarPCNextWire.val)

        -- Trigger next stage
        scalarGo5 <== scalarGo4.val
        scalarWarpId5 <== scalarWarpId4.val
        scalarState5 <== scalarState4.val
        scalarIsSusp5 <== scalarIsSusp4.val
        scalarInstr5 <== scalarInstr4.val
        scalarAbort5 <== scalarAbort4.val .||. affineAbortWire.val

      -- Stage 5: Writeback & Resume
      -- ===========================

      -- For resuming thread after scalar store latency
      scalarResumeGo <- makeDReg false
      scalarResumeWarpId <- makeReg dontCare

      always do
        when scalarGo5.val do
          -- Update PC
          let doUpdatePC =
                andList [ inv scalarIsSusp5.val
                        , inv scalarRetry5.val
                        , inv scalarAbort5.val ]
          when doUpdatePC do
            let newPC = old scalarPCNextWire.val
            let newState = scalarState5.val {
                    simtPC = newPC
                  , simtNestLevel = scalarNestLevel5.val }
            sequence_
              [ stateMem.store scalarWarpId5.val newState
              | stateMem <- stateMemsB ]
            -- Update PCC
            when enableCHERI do
              when (delay false scalarPCCNextWire.active) do
                let newPCC = upper $ pack $ toMem $ old scalarPCCNextWire.val
                sequence_
                  [ pccMem.store scalarWarpId5.val newPCC
                  | pccMem <- pccMemsB ]

          -- Reschedule warp
          if scalarAbort5.val .||. (inv scalarIsSusp5.val .&&.
                                    inv scalarRetry5.val .&&.
                                    inv scalarTableB.out)
            then do
              -- If instr was not scalarisable
              -- or next instr is not predicted scalarisable,
              -- then move warp to vector pipeline
              (warpQueue!scalarWarpId5.val) <== true
              scalarUnitWarpCount.decrBy 1
            else do
              -- Otherwise, keep warp in scalar pipeline
              (scalarQueue!scalarWarpId5.val) <== true

          -- Suspend warp
          when scalarSuspend5.val do
            ((head suspBits)!scalarWarpId5.val) <== true

        -- Write to reg file
        if delay false scalarResultWire.active .&&. inv scalarAbort5.val
          then do
            let dest = (scalarWarpId5.val, dst scalarInstr5.val)
            regFile.storeScalar dest
              ScalarVal { val = false # old scalarResultWire.val
                        , stride = old scalarResultStrideWire.val }
            when enableCHERI do
              capRegFile.storeScalar dest
                ScalarVal { val = old scalarResultCapWire.val
                          , stride = 0 }
          else do
            let resumeReqs = inputs.simtScalarResumeReqs
            when resumeReqs.canPeek do
              -- Process resume request
              resumeReqs.consume
              let (info, req) = resumeReqs.peek
              let dest = (info.warpId, info.destReg)
              when (info.destReg .!=. 0) do
                -- Affine vectors not yet allowed on resume path
                regFile.storeScalar dest
                  ScalarVal { val = false # req.resumeReqData, stride = 0 }
                when enableCHERI do
                  capRegFile.storeScalar dest
                    ScalarVal { val = if req.resumeReqCap.valid
                                        then req.resumeReqCap.val
                                        else nullCapMemMetaVal
                              , stride = 0 }
              -- Trigger warp resumption
              scalarResumeGo <== true
              scalarResumeWarpId <== info.warpId

        -- Resume warp after scalar reg file store latency has elapsed
        when scalarResumeGo.val do
          ((head suspBits)!scalarResumeWarpId.val) <== false

    -- Handle barrier release
    -- ======================

    -- Warps per block (a block is a group of threads that synchronise
    -- on a barrier). A value of zero indicates all warps.
    warpsPerBlock :: Reg (Bit SIMTLogWarps) <- makeReg 0

    -- Mask that identifies a block of threads
    barrierMask :: Reg (Bit SIMTWarps) <- makeReg ones

    makeBarrierReleaseUnit
      BarrierReleaseIns {
        relWarpsPerBlock = warpsPerBlock.val
      , relBarrierMask = barrierMask.val
      , relBarrierVec = fromBitList (map (.val) barrierBits)
      , relAction = \warpId -> do
          when (inv excGlobal.val) do
            -- Clear barrier bit
            barrierBits!warpId <== false
            decrBy barrierCount 1
            -- Insert back into warp queue
            (warpQueue!warpId) <== true
      , relDisable = regSpillMode.val
      }

    -- Handle management requests
    -- ==========================

    -- Address of kernel code ptr and args, as set by CPU
    kernelAddrReg :: Reg (Bit 32) <- makeReg dontCare

    always do
      when (inputs.simtMgmtReqs.canPeek) do
        let req = inputs.simtMgmtReqs.peek
        -- Is pipeline busy?
        let busy = startReg.val.valid .|. pipelineActive.val
        -- Write instruction
        when (req.simtReqCmd .==. simtCmd_WriteInstr) do
          dynamicAssert (inv busy)
            "SIMT pipeline: writing instruction while pipeline busy"
          instrMemA.store (toInstrAddr req.simtReqAddr) (req.simtReqData)
          inputs.simtMgmtReqs.consume
        -- Start pipeline
        when (req.simtReqCmd .==. simtCmd_StartPipeline) do
          when (inv busy) do
            startReg <== some (req.simtReqAddr)
            kernelAddrReg <== req.simtReqData
            inputs.simtMgmtReqs.consume
        -- Set warps per block
        when (req.simtReqCmd .==. simtCmd_SetWarpsPerBlock) do
          dynamicAssert (inv busy)
            "SIMT pipeline: setting warps per block while pipeline busy"
          let n :: Bit SIMTLogWarps = truncateCast req.simtReqData
          warpsPerBlock <== n
          barrierMask <== n .==. 0 ? (ones, (1 .<<. n) - 1)
          inputs.simtMgmtReqs.consume
        -- Read stat counter
        when (req.simtReqCmd .==. simtCmd_AskStats) do
          let statId = unpack (truncate req.simtReqAddr)
          when (inv pipelineActive.val .&&. kernelRespQueue.notFull) do
            let resp = select
                  [ statId .==. simtStat_Cycles --> cycleCount.val
                  , statId .==. simtStat_Instrs --> instrCount.val
                  , statId .==. simtStat_VecRegs -->
                      zeroExtend regFile.maxVecRegs
                  , statId .==. simtStat_CapVecRegs -->
                      zeroExtend capRegFile.maxVecRegs
                  , statId .==. simtStat_ScalarisableInstrs -->
                      scalarisableInstrCount.val
                  , statId .==. simtStat_Retries --> retryCount.val
                  , statId .==. simtStat_SuspBubbles --> suspCount.val
                  , statId .==. simtStat_ScalarSuspBubbles -->
                      scalarSuspCount.val
                  , statId .==. simtStat_ScalarAborts -->
                      scalarAbortCount.val
                  , statId .==. simtStat_DRAMAccesses -->
                      dramAccessCount.val
                  , statId .==. simtStat_TotalVecRegs -->
                      regFile.totalVecRegs
                  , statId .==. simtStat_TotalCapVecRegs -->
                      capRegFile.totalVecRegs
                  , statId .==. simtStat_SBLoadHit -->
                      sbLoadHitCount.val
                  , statId .==. simtStat_SBLoadMiss -->
                      sbLoadMissCount.val
                  , statId .==. simtStat_SBCapLoadHit -->
                      sbCapLoadHitCount.val
                  , statId .==. simtStat_SBCapLoadMiss -->
                      sbCapLoadMissCount.val
                  ]
            enq kernelRespQueue
              (if c.enableStatCounters then resp else zero)
            inputs.simtMgmtReqs.consume

    -- Pipeline outputs
    return
      SIMTPipelineOuts {
        simtMgmtResps = toStream kernelRespQueue
      , simtCurrentWarpId = zeroExtendCast warpId5
      , simtKernelAddr = kernelAddrReg.val
      , simtInstrInfo =
          SIMTPipelineInstrInfo {
            destReg = if unspill5 then unspillReg5 else dst instr5
          , warpId = warpId5
          , regFileId =
              if unspill5
                then unspillTo5 ? (regFileCapMeta, regFileInt)
                else regFileIntCap
          }
      , simtScalarInstrInfo =
          SIMTPipelineInstrInfo {
            destReg = dst scalarInstr4.val
          , warpId = scalarWarpId4.val
          , regFileId = regFileIntCap
          }
      , simtScalarisedOpB = scalarisedOperandB5
      }

-- Barrier release unit
-- ====================

-- This logic looks for threads in a block that have all entered a
-- barrier, and then releases them.

-- | Inputs to barrier release unit
data BarrierReleaseIns =
  BarrierReleaseIns {
    relWarpsPerBlock :: Bit SIMTLogWarps
    -- ^ Number of warps per block of synchronising threads
  , relBarrierMask :: Bit SIMTWarps
    -- ^ Mask that identifies a block of threads
  , relBarrierVec :: Bit SIMTWarps
    -- ^ Bit vector denoting warps currently in barrier
  , relAction :: Bit SIMTLogWarps -> Action ()
    -- ^ Action to perform on a release
  , relDisable :: Bit 1
    -- ^ Signal to disable barrier release
  }

-- | Barrier release unit
makeBarrierReleaseUnit :: BarrierReleaseIns -> Module ()
makeBarrierReleaseUnit ins = do
  -- Mask that identifies a block of threads
  barrierMask :: Reg (Bit SIMTWarps) <- makeReg ones

  -- Shift register for barrier release logic
  barrierShiftReg :: Reg (Bit SIMTWarps) <- makeReg dontCare

  -- For release state machine
  releaseState :: Reg (Bit 2) <- makeReg 0

  -- Warp counters for release logic
  releaseWarpId :: Reg (Bit SIMTLogWarps) <- makeReg dontCare
  releaseWarpCount :: Reg (Bit SIMTLogWarps) <- makeReg dontCare

  -- Is the current block of threads ready for release?
  releaseSuccess :: Reg (Bit 1) <- makeReg false

  -- Barrier release state machine
  always do
    -- Load barrier bits into shift register
    when (releaseState.val .==. 0) do
      -- Load barrier bits into shift register
      barrierShiftReg <== ins.relBarrierVec
      -- Intialise warp id (for iterating over all warps)
      releaseWarpId <== 0
      -- Enter shift state
      releaseState <== 1

    -- Check if head block has synced
    when (releaseState.val .==. 1) do
      -- Have all warps in the block entered the barrier?
      releaseSuccess <==
        (barrierShiftReg.val .&. ins.relBarrierMask) .==. ins.relBarrierMask
      -- Initialise warp count (for iterating over warps in a block)
      releaseWarpCount <== 1
      -- Move to next state
      if barrierShiftReg.val .==. 0
        then do releaseState <== 0
        else do releaseState <== 2

    -- Shift and release
    when (releaseState.val .==. 2 .&&. inv ins.relDisable) do
      -- Release warp
      when (releaseSuccess.val) do
        relAction ins (releaseWarpId.val)
      -- Shift
      barrierShiftReg <== barrierShiftReg.val .>>. (1 :: Bit 1)
      -- Move to next warp
      releaseWarpId <== releaseWarpId.val + 1
      releaseWarpCount <== releaseWarpCount.val + 1
      -- Move back to state 1 when finished with block
      when (releaseWarpCount.val .==. ins.relWarpsPerBlock) do
        releaseState <== 1

-- Note [Dynamic register spilling]
-- ================================

-- The size of the physical vector register file may be set such that
-- it is smaller than the number of architectural registers. Most of
-- the time, this may not be a problem due to register file compression
-- (using scalarisation), but when compression fails to work well for
-- a particular workload, or when the size of the physical register
-- file is simply too small, we must deal with overflow.
--
-- Dynamic register spilling is a modification to the SIMT pipeline
-- that spills registers to DRAM, and fetches them as and when
-- required by an instruction.
--
-- The pipeline modifications for the spilling (eviction) of a
-- register are as follows:
--
-- Warp Scheduler: The "pipeline spill mode" is triggered when the
-- utilisation of the physical register file exceeds a threshold. In
-- this mode, and a warp is selected fairly from the union of the warp
-- queue, scalar warp queue, and the barrier queue. The threshold is
-- chosen such that every inflight instruction has space to allocate
-- at least one register in the register file.
--
-- Operand Fetch: In this stage, we decide which register to evict.
-- Various policies are possible here; we leave it abstract for the
-- purpose of this summary. However, there is one key condition on
-- the register chosen for eviction: it must not be needed by the next
-- instruction to be executed by the current warp. This guarantees
-- forward progress, avoiding the livelock case where a register is
-- continually fetched and subsequently evicted before being used,
-- provided that the physical register file is big enough to hold at
-- least three vectors for each warp.
--
-- Execute Stage: Here, we issue a write of the chosen vector to a
-- small region of DRAM dedicated for register spilling.
--
-- Writeback Stage: Here, we mark the register as "evicted" in the
-- register file, such that if the register is ever needed by a future
-- instruction, we can detect that we need to fetch it from DRAM.
--
-- The pipeline modifications for the unspilling (fetching) of a
-- register are as follows:
--
-- Operand Fetch: In this stage, we detect the case where an
-- instruction needs a register that has been evicted to DRAM, and we
-- remember this register for future stages.
-- 
-- Execute stage: Here, we issue a load to the register file spill
-- region in DRAM, and arrange for the result to be written to the
-- correct place in the register file.  The warp is then suspended, in
-- the same way as a load instruction, and the PC of the current
-- instruction is not incremented.
