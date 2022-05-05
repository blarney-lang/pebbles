module Pebbles.Pipeline.SIMT
  ( -- Pipeline configuration
    SIMTPipelineConfig(..)
    -- Pipeline inputs and outputs
  , SIMTPipelineIns(..)
  , SIMTPipelineOuts(..)
    -- Instruction info for multi-cycle instructions
  , SIMTPipelineInstrInfo
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
import Blarney.TaggedUnion
import Blarney.QuadPortRAM
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
  , scalarUnitDecodeStage :: [(String, tag)]
    -- ^ Decode table for scalar unit
  , scalarUnitExecuteStage :: State -> Module ExecuteStage
    -- ^ Execute stage for scalar unit
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

    -- Compute field selector functions from decode table
    let selMap = matchSel (c.decodeStage)

    -- Functions for extracting register ids from an instruction
    let srcA :: Instr -> RegId = getBitFieldSel selMap "rs1"
    let srcB :: Instr -> RegId = getBitFieldSel selMap "rs2"
    let dst  :: Instr -> RegId = getBitFieldSel selMap "rd"

    -- Queue of active warps for vector pipeline
    warpQueue :: Queue (Bit SIMTLogWarps) <- makeSizedQueue SIMTLogWarps

    -- Queue of warps moving from vector pipeline to scalar pipeline
    toScalarQueue :: Queue (Bit SIMTLogWarps) <-
      makeSizedQueue SIMTLogWarps

    -- Queue of warps moving from scalar pipeline to vector pipeline
    toVectorQueue :: Queue (Bit SIMTLogWarps) <-
      makeSizedQueue SIMTLogWarps

    -- One block RAM of thread states per lane
    (stateMemsA, stateMemsB) :: ([RAM (Bit SIMTLogWarps) SIMTThreadState],
                                 [RAM (Bit SIMTLogWarps) SIMTThreadState]) <-
      unzip <$> replicateM SIMTLanes makeQuadRAM

    -- One program counter capability RAM (meta-data only) per lane
    pccMems :: [RAM (Bit SIMTLogWarps) CapMem] <-
      replicateM SIMTLanes $
        if enableCHERI && not (c.useSharedPCC)
          then makeDualRAM
          else return nullRAM

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

    -- Register file
    regFile :: SIMTRegFile 32 <-
      if c.useRegFileScalarisation
        then makeSIMTScalarisingRegFile
               c.useAffineScalarisation c.useScalarUnit 0
        else makeSIMTRegFile loadLatency Nothing

    -- Capability register file (meta-data only)
    capRegFile :: SIMTRegFile CapMemMetaWidth <-
      if enableCHERI
        then
          if c.useCapRegFileScalarisation
            then makeSIMTScalarisingRegFile
                   False False nullCapMemMetaVal
            else makeSIMTRegFile loadLatency (Just nullCapMemMetaVal)
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
    go1 :: Reg (Bit 1) <- makeDReg false
    go3 :: Reg (Bit 1) <- makeDReg false
    go4 :: Reg (Bit 1) <- makeDReg false

    -- Warp id register, for each stage
    warpId1 :: Reg (Bit SIMTLogWarps) <- makeReg dontCare
    warpId3 :: Reg (Bit SIMTLogWarps) <- makeReg dontCare
    warpId4 :: Reg (Bit SIMTLogWarps) <- makeReg dontCare

    -- Thread state, for each stage
    state3 :: Reg SIMTThreadState <- makeReg dontCare
    state4 :: Reg SIMTThreadState <- makeReg dontCare

    -- Active thread mask
    activeMask3 :: Reg (Bit SIMTLanes) <- makeReg dontCare
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

    -- Program counter capability registers
    pccShared :: Reg CapPipe <- makeReg dontCare
    pcc3 :: Reg Cap <- makeReg dontCare

    -- Stat counters
    cycleCount :: Reg (Bit 32) <- makeReg 0
    instrCount :: Reg (Bit 32) <- makeReg 0

    -- Triggers from each lane to increment instruction count
    incInstrCountRegs :: [Reg (Bit 1)] <- replicateM SIMTLanes (makeDReg 0)

    -- Indicates that current instruction is scalarisable
    instrScalarisable5 <- makeReg false

    -- Function to convert from 32-bit PC to instruction address
    let toInstrAddr :: Bit 32 -> Bit t_logInstrs =
          \pc -> truncateCast (slice @31 @2 pc)

    -- Pipeline Initialisation
    -- =======================

    -- Register to trigger pipeline initialisation at some PC
    startReg :: Reg (Option (Bit 32)) <- makeReg none

    -- Warp id counter, to initialise PC of each thread
    warpIdCounter :: Reg (Bit SIMTLogWarps) <- makeReg 0

    -- Is the pipeline active?
    pipelineActive :: Reg (Bit 1) <- makeReg false

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

        -- Initialise PCC per lane
        sequence_
          [ do stateMem.store (warpIdCounter.val) initState
               if enableCHERI
                 then do
                   let initPCC = almightyCapMemVal -- TODO: constrain
                   pccMem.store (warpIdCounter.val) initPCC
                 else return ()
          | (stateMem, pccMem) <- zip stateMemsA pccMems ]

        when (warpIdCounter.val .==. 0) do
          -- Register file initialisation
          regFile.init
          capRegFile.init

          -- Intialise PCC per kernel
          pccShared <== almightyCapPipeVal -- TODO: constrain

          -- Reset various state
          excGlobal <== false
          sequence_ [e <== false | e <- excLocals]
          setCount barrierCount 0
          sequence_ [r <== false | r <- barrierBits]

        -- Insert into warp queue
        dynamicAssert (warpQueue.notFull)
          "SIMT warp queue overflow during initialisation"
        enq warpQueue (warpIdCounter.val)

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

    -- Stat counters
    -- =============

    if c.enableStatCounters
      then
        always do
          when (pipelineActive.val) do
            -- Increment cycle count
            cycleCount <== cycleCount.val + 1

            -- Increment instruction count
            let instrIncs :: [Bit 32] =
                  map zeroExtend (map (.val) incInstrCountRegs)
            let instrInc = tree1 (\a b -> reg 0 (a+b)) instrIncs
            instrCount <== instrCount.val + instrInc
      else
        return ()

    -- ===============
    -- Vector Pipeline
    -- ===============

    -- Stage 0: Warp Scheduling
    -- ========================

    -- Queue of warps that have left a barrier (half throughput)
    releaseQueue :: Queue (Bit SIMTLogWarps) <-
      makeSizedQueueConfig
        SizedQueueConfig {
          sizedQueueLogSize = SIMTLogWarps
        , sizedQueueBuffer = makeShiftQueue 1
        }

    -- Stream of warps to schedule next
    warpStream <- makeFairMerger $
         [toStream releaseQueue, toStream warpQueue]
      ++ [toStream toVectorQueue | c.useScalarUnit]

    always do
      -- Load state for next warp on each lane
      forM_ stateMemsA \stateMem -> do
        stateMem.load (warpStream.peek)

      -- Load PCC for next warp on each lane
      if enableCHERI && not (c.useSharedPCC)
        then do
          forM_ pccMems \pccMem -> do
            pccMem.load (warpStream.peek)
        else return ()

      -- Buffer warp id for stage 1
      warpId1 <== warpStream.peek

      -- Trigger stage 1
      when (warpStream.canPeek .&&. pipelineActive.val) do
        warpStream.consume
        go1 <== true

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
          [iterateN (stage1Substages-1) buffer (mem.out) | mem <- pccMems]
  
    -- State and PCC of leader
    let state2 = buffer (states2_tmp ! leaderIdx)
    let pcc2   = buffer (pccs2_tmp ! leaderIdx)

    -- Stat and PCC of all threads
    let stateMemOuts2 = map buffer states2_tmp
    let pccs2 = map buffer pccs2_tmp

    -- Trigger stage 2
    let warpId2 = iterateN stage1Substages buffer (warpId1.val)
    let go2 = iterateN stage1Substages (delay 0) (go1.val)

    -- Stage 2: Instruction Fetch
    -- ==========================

    always do
      -- Compute active thread mask
      let activeList =
            [ state2 === s .&&.
                if enableCHERI && not (c.useSharedPCC)
                  then upper pcc2 .==. (upper pcc :: CapMemMeta)
                  else true
            | (s, pcc) <- zip stateMemOuts2 pccs2]
      let activeMask :: Bit SIMTLanes = fromBitList activeList
      activeMask3 <== activeMask

      -- Assert that at least one thread in the warp must be active
      when go2 do
        dynamicAssert (orList activeList)
          "SIMT pipeline error: no active threads in warp"

      -- Issue load to instruction memory
      let pc = state2.simtPC
      instrMemA.load (toInstrAddr pc)

      -- Trigger stage 3
      warpId3 <== warpId2
      state3 <== state2
      pcc3 <==
        let pccToUse = if c.useSharedPCC then pccShared.val
                                         else fromMem (unpack pcc2)
            cap = setAddr pccToUse pc
         in decodeCapPipe (cap.value)
      go3 <== go2

    -- Stage 3: Operand Fetch
    -- ======================

    let pcc4 = delay dontCare (pcc3.val)

    always do
      -- Fetch operands from register file
      regFile.loadA (warpId3.val, srcA instrMemA.out)
      regFile.loadB (warpId3.val, srcB instrMemA.out)

      -- Fetch capability meta-data from register file
      capRegFile.loadA (warpId3.val, srcA instrMemA.out)
      capRegFile.loadB (warpId3.val, srcB instrMemA.out)

      -- Is any thread in warp suspended?
      -- (In future, consider only suspension bits of active threads)
      let isSusp3 = orList [map (.val) regs ! warpId3.val | regs <- suspBits]
      isSusp4 <== isSusp3

      -- Check PCC
      case c.checkPCCFunc of
        -- CHERI disabled; no check required
        Nothing -> return ()
        -- Check PCC
        Just checkPCC -> do
          let table = checkPCC (pcc3.val)
          let exception = orList [cond | (cond, _) <- table]
          when (go3.val) do
            when exception do
              head excLocals <== true
              let trapCode = priorityIf table (excCapCode 0)
              display "SIMT pipeline: PCC exception: code=" trapCode

      -- Trigger stage 4
      warpId4 <== warpId3.val
      activeMask4 <== activeMask3.val
      instr4 <== instrMemA.out
      state4 <== state3.val
      go4 <== go3.val

    -- Stage 4: Operand Latch
    -- ======================

    -- Delay given signal by register file load latency
    let loadDelay :: Bits a => a -> a
        loadDelay inp = iterateN (loadLatency - 1) (delay zero) inp

    -- Decode instruction
    let (tagMap4, fieldMap4) = matchMap False (c.decodeStage)
                                 (loadDelay instr4.val)

    -- Stage 5 register operands
    let vecRegA5 = old regFile.outA
    let vecRegB5 = old regFile.outB

    -- Stage 5 capability register operands
    let getCapReg intReg capReg =
          old $ decodeCapMem (capReg # intReg)
    let vecCapRegA5 = V.zipWith getCapReg regFile.outA capRegFile.outA
    let vecCapRegB5 = V.zipWith getCapReg regFile.outB capRegFile.outB

    -- Stage 5 register B or immediate
    let getRegBorImm reg = old $
          if Map.member "imm" fieldMap4
            then let imm = getBitField fieldMap4 "imm"
                 in  imm.valid ? (imm.val, reg)
            else reg
    let vecRegBorImm5 = V.map getRegBorImm regFile.outB

    -- Propagate signals to stage 5
    let pcc5 = old (loadDelay pcc4)
    let isSusp5 = old (loadDelay isSusp4.val)
    let warpId5 = old (loadDelay warpId4.val)
    let activeMask5 = old (loadDelay activeMask4.val)
    let instr5 = old (loadDelay instr4.val)
    let state5 = old (loadDelay state4.val)
    let go5 = delay false (loadDelay go4.val)

    -- Buffer the decode tables
    let tagMap5 = Map.map old tagMap4

    -- Determine if field is available in current instruction
    let isFieldInUse fld fldMap =
          case Map.lookup fld fldMap of
            Nothing -> false
            Just opt -> opt.valid

    -- Determine if this instruction is scalarisable
    when (c.useRegFileScalarisation && c.useScalarUnit) do
      -- XXX: affine vectors not yet allowed in scalar pipeline
      let isUniform scalar = scalar.valid .&&. scalar.val.stride .==. 0
      let isOpScalarisable = orList
            [ Map.findWithDefault false op tagMap4
            | op <- c.scalarUnitAllowList ]
      always do
        instrScalarisable5 <== andList
          [ loadDelay (activeMask4.val .==. ones)
          , isFieldInUse "rs1" fieldMap4 ? (isUniform regFile.scalarA, true)
          , isFieldInUse "rs2" fieldMap4 ? (isUniform regFile.scalarB, true)
          , isOpScalarisable
          ]

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
        -- Update scalar prediction table
        when (inv isSusp5) do
          scalarTableA.store (toInstrAddr state5.simtPC)
                             instrScalarisable5.val
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

    -- Vector lane definition
    let makeLane makeExecStage threadActive suspMask regA regB regBorImm
                 capRegA capRegB stateMem incInstrCount pccMem
                 excLocal laneId resultWire resultCapWire pcChange = do

          -- Per lane interfacing
          pcNextWire :: Wire (Bit 32) <-
            makeWire (state5.simtPC + 4)
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
            , opBorImm = regBorImm
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
                if enableCHERI
                  then pccNextWire <== pccNew
                  else return ()
            , resultCap = WriteOnly \cap ->
                            when destNonZero do
                              let capMem = pack (toMem cap)
                              resultWire <== lower capMem
                              resultCapWire <== upper capMem
            }

          always do
            -- Did PC change?
            pcChange <== pcNextWire.active

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
              when (pccNextWire.active) do
                pccMem.store warpId5 (pack (toMem pccNextWire.val))

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
               <*> ZipList pccMems
               <*> ZipList excLocals
               <*> ZipList [0..]
               <*> ZipList resultWires
               <*> ZipList resultCapWires
               <*> ZipList pcChangeRegs6

    -- Stage 6: Writeback
    -- ==================

    always do
      let warpId6 = old warpId5
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
      let resumeVecCap = fromList
            [ Option (req.valid .&&. resumeInfo.destReg .!=. 0 .&&.
                       inv excGlobal.val)
                     (if req.val.resumeReqCap.valid
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

      -- Write to register file
      when (handleExecute .||. inputs.simtResumeReqs.canPeek) do
        regFile.store writeIdx writeVec
        when enableCHERI do
          capRegFile.store writeIdx writeCapVec
      -- Handle thread resumption
      when (inv handleExecute .&&. inputs.simtResumeReqs.canPeek) do
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
            (handleExecute .||. inputs.simtResumeReqs.canPeek)
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
                  , toScalarQueue.notFull
                  ]
                else false
        if putInScalarQueue .&&. toScalarQueue.notFull
          then do
            toScalarQueue.enq warpId6
          else do
            dynamicAssert (warpQueue.notFull) "SIMT warp queue overflow"
            warpQueue.enq warpId6

    -- ===============
    -- Scalar Pipeline
    -- ===============

    -- XXX: CHERI is not yet supported in the SIMT scalar pipeline
    staticAssert (c.useScalarUnit <= not enableCHERI)
      "CHERI is not yet supported in the SIMT scalar pipeline"

    -- Scalar pipeline stage trigger signals
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

    -- Warp suspension status, per pipeline stage
    scalarIsSusp3 :: Reg (Bit 1) <- makeReg dontCare
    scalarIsSusp4 :: Reg (Bit 1) <- makeReg dontCare
    scalarIsSusp5 :: Reg (Bit 1) <- makeReg dontCare

    -- Instruciton operand registers
    scalarOpA4 :: Reg (Bit 32) <- makeReg dontCare
    scalarOpB4 :: Reg (Bit 32) <- makeReg dontCare
    scalarOpBOrImm4 :: Reg (Bit 32) <- makeReg dontCare

    -- Abort scalar pipeline if instruction turns out not to be scalarisable
    scalarAbort4 :: Reg (Bit 1) <- makeReg dontCare
    scalarAbort5 :: Reg (Bit 1) <- makeReg dontCare

    -- Retry/suspend wires from execute stage
    scalarSuspend5 :: Reg (Bit 1) <- makeDReg false
    scalarRetry5 :: Reg (Bit 1) <- makeDReg false

    when c.useScalarUnit do

      -- Scalar warp queue
      scalarQueue :: Queue (Bit SIMTLogWarps) <- makeSizedQueue SIMTLogWarps

      -- Merge scalar pipeline warp queues
      scalarWarps <- makeFairMerger
        [toStream scalarQueue, toStream toScalarQueue]

      -- Stage 0: Warp Scheduling
      -- ========================

      always do
        -- Next warp
        let warpId = scalarWarps.peek

        -- Lookup warp's PC
        (head stateMemsB).load warpId

        when scalarWarps.canPeek do
          scalarWarps.consume
          -- Trigger stage 1
          scalarGo1 <== true
          scalarWarpId1 <== warpId

      -- Stage 1: Instruction Fetch
      -- ==========================

      always do
        let state = (head stateMemsB).out

        -- Load next instruction
        instrMemB.load (toInstrAddr state.simtPC)

        -- Trigger next stage
        scalarGo2 <== scalarGo1.val
        scalarWarpId2 <== scalarWarpId1.val
        scalarState2 <== state

      -- Stage 2: Operand Fetch
      -- ======================

      always do
        -- Fetch operands from register file
        regFile.loadScalarC (scalarWarpId2.val, srcA instrMemB.out)
        regFile.loadScalarD (scalarWarpId2.val, srcB instrMemB.out)
        -- Is any thread in warp suspended?
        scalarIsSusp3 <== orList [ map (.val) regs ! scalarWarpId2.val
                                 | regs <- suspBits ]
        -- Trigger next stage
        scalarGo3 <== scalarGo2.val
        scalarWarpId3 <== scalarWarpId2.val
        scalarState3 <== scalarState2.val
        scalarInstr3 <== instrMemB.out

      -- Stage 3: Operand Latch
      -- ======================


      -- Decode instruction
      let (scalarTagMap3, scalarFieldMap3) =
            matchMap False c.scalarUnitDecodeStage scalarInstr3.val

      -- Use "imm" field if valid, otherwise use register b
      -- XXX: affine vectors not yet allowed in scalar pipeline
      let bOrImm = if Map.member "imm" scalarFieldMap3
                     then let imm = getBitField scalarFieldMap3 "imm"
                          in imm.valid ? (imm.val, regFile.scalarD.val.val)
                     else regFile.scalarD.val.val

      always do
        -- Trigger next stage
        scalarGo4 <== scalarGo3.val
        -- Latch operands
        scalarOpA4 <== regFile.scalarC.val.val  -- XXX: assume uniform
        scalarOpB4 <== regFile.scalarD.val.val  -- XXX: assume uniform
        scalarOpBOrImm4 <== bOrImm
        scalarWarpId4 <== scalarWarpId3.val
        scalarState4 <== scalarState3.val
        scalarInstr4 <== scalarInstr3.val
        scalarIsSusp4 <== scalarIsSusp3.val
        -- Is instruction in the allow list for the scalar unit?
        let isOpScalarisable = orList
              [ Map.findWithDefault false op scalarTagMap3
              | op <- c.scalarUnitAllowList ]
        -- Abort if any used operand is a vector
        -- XXX: affine vectors not yet allowed in scalar pipeline
        scalarAbort4 <== orList
              [ isFieldInUse "rs1" scalarFieldMap3 .&&.
                  (inv regFile.scalarC.valid .||.
                     regFile.scalarC.val.stride .!=. 0)
              , isFieldInUse "rs2" scalarFieldMap3 .&&.
                  (inv regFile.scalarD.valid .||.
                     regFile.scalarD.val.stride .!=. 0)
              , inv isOpScalarisable ]

      -- Stage 4: Execute & Suspend
      -- ==========================

      -- Execute stage wires
      scalarPCNextWire :: Wire (Bit 32) <-
        makeWire (scalarState4.val.simtPC + 4)
      scalarResultWire :: Wire (Bit 32) <- makeWire dontCare

      let scalarTagMap4 = Map.map old scalarTagMap3

      -- Instantiate execute stage
      execStage <- c.scalarUnitExecuteStage
        State {
          instr = scalarInstr4.val
        , opA = scalarOpA4.val
        , opB = scalarOpB4.val
        , opBorImm = scalarOpBOrImm4.val
        , opAIndex = srcA scalarInstr4.val
        , opBIndex = srcB scalarInstr4.val
        , resultIndex = dst scalarInstr4.val
        , pc = ReadWrite scalarState4.val.simtPC \pcNew -> do
                 scalarPCNextWire <== pcNew
        , result = WriteOnly \x ->
                     when (dst scalarInstr4.val .!=. 0) do
                       scalarResultWire <== x
        , suspend = do scalarSuspend5 <== true
        , retry = do scalarRetry5 <== true
        , opcode = packTagMap scalarTagMap4
        , trap = \code -> do
            display "Scalar unit trap: code=" code
                    " pc=0x" (formatHex 8 scalarState4.val.simtPC)
        , capA = dontCare
        , capB = dontCare
        , pcc = dontCare
        , pccNew = WriteOnly \pccNew -> return ()
        , resultCap = WriteOnly \cap -> return ()
        }

      always do
        -- Invoke execute stage
        when (scalarGo4.val .&&. inv scalarIsSusp4.val .&&.
                inv scalarAbort4.val) do
          execStage.execute

        -- Lookup scalar prediction table
        scalarTableB.load (toInstrAddr scalarPCNextWire.val)

        -- Trigger next stage
        scalarGo5 <== scalarGo4.val
        scalarWarpId5 <== scalarWarpId4.val
        scalarState5 <== scalarState4.val
        scalarIsSusp5 <== scalarIsSusp4.val
        scalarInstr5 <== scalarInstr4.val
        scalarAbort5 <== scalarAbort4.val

      -- Stage 5: Writeback & Resume
      -- ===========================

      always do
        when scalarGo5.val do
          -- Update PC
          let doUpdatePC =
                andList [ inv scalarIsSusp5.val
                        , inv scalarRetry5.val
                        , inv scalarAbort5.val ]
          when doUpdatePC do
            let newPC = old scalarPCNextWire.val
            let newState = scalarState5.val { simtPC = newPC }
            sequence_
              [ stateMem.store scalarWarpId5.val newState
              | stateMem <- stateMemsB ]

          -- Reschedule warp
          if scalarAbort5.val .||. inv scalarTableB.out
            then do
              -- If instr was not scalarisable
              -- or next instr is not predicted scalarisable,
              -- then move warp to vector pipeline
              dynamicAssert toVectorQueue.notFull
                            "SIMT scalar pipeline: toVectorQueue overflow"
              toVectorQueue.enq scalarWarpId5.val
            else do
              -- Otherwise, keep warp in scalar pipeline
              dynamicAssert scalarQueue.notFull
                            "SIMT scalar pipeline: scalarQueue overflow"
              scalarQueue.enq scalarWarpId5.val

          -- Suspend warp
          when scalarSuspend5.val do
            ((head suspBits)!scalarWarpId5.val) <== true

        -- Write to reg file
        if delay false scalarResultWire.active
          then do
            let dest = (scalarWarpId5.val, dst scalarInstr5.val)
            -- XXX: affine vectors not yet allowed in scalar pipeline
            regFile.storeScalar dest
              ScalarVal { val = old scalarResultWire.val, stride = 0 }
          else do
            let resumeReqs = inputs.simtScalarResumeReqs
            when resumeReqs.canPeek do
              -- Process resume request
              resumeReqs.consume
              let (info, req) = resumeReqs.peek
              let dest = (info.warpId, info.destReg)
              -- XXX: affine vectors not yet allowed in scalar pipeline
              regFile.storeScalar dest
                ScalarVal { val = req.resumeReqData, stride = 0 }
              -- Resume warp
              ((head suspBits)!info.warpId) <== false

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
            dynamicAssert (releaseQueue.notFull)
              "SIMT release queue overflow"
            enq releaseQueue warpId
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
                  [ statId .==. simtStat_Cycles  --> cycleCount.val
                  , statId .==. simtStat_Instrs  --> instrCount.val
                  , statId .==. simtStat_VecRegs -->
                      zeroExtend regFile.maxVecRegs
                  , statId .==. simtStat_CapVecRegs -->
                      zeroExtend capRegFile.maxVecRegs
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
            destReg = dst instr5
          , warpId = warpId5
          }
      , simtScalarInstrInfo =
          SIMTPipelineInstrInfo {
            destReg = dst scalarInstr5.val
          , warpId = scalarWarpId5.val
          }
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
    when (releaseState.val .==. 2) do
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
