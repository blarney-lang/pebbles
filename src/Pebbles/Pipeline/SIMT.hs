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
-- There are 8 pipeline stages:
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
  , useCapRegFileScalarisation :: Bool
    -- ^ Use scalarising register file for capabilities?
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
      -- ^ Resume requests for multi-cycle instructions
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
      -- ^ Info for instruction currently in execute stage
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

    -- Queue of active warps
    warpQueue :: Queue (Bit SIMTLogWarps) <- makeSizedQueue SIMTLogWarps

    -- One block RAM of thread states per lane
    stateMems ::  [RAM (Bit SIMTLogWarps) SIMTThreadState ] <-
      replicateM SIMTLanes makeDualRAM

    -- One program counter capability RAM (meta-data only) per lane
    pccMems :: [RAM (Bit SIMTLogWarps) CapMem] <-
      replicateM SIMTLanes $
        if enableCHERI && not (c.useSharedPCC)
          then makeDualRAM
          else return nullRAM

    -- Instruction memory
    instrMem :: RAM (Bit t_logInstrs) Instr <-
      makeDualRAMCore (c.instrMemInitFile)

    -- Suspension bit for each thread
    suspBits :: [[Reg (Bit 1)]] <-
      replicateM SIMTLanes (replicateM SIMTWarps (makeReg false))

    -- Register file load latency
    let loadLatency =
          if c.useCapRegFileScalarisation || c.useRegFileScalarisation
            then basicSIMTScalarisingRegFile_loadLatency
            else if enableCHERI then 2 else 1

    -- Register file
    regFile :: SIMTRegFile (Bit 32) <-
      if c.useRegFileScalarisation
        then makeBasicSIMTScalarisingRegFile 0
        else makeSimpleSIMTRegFile loadLatency Nothing

    -- Capability register file (meta-data only)
    capRegFile :: SIMTRegFile CapMemMeta <-
      if enableCHERI
        then
          if c.useCapRegFileScalarisation
            then makeBasicSIMTScalarisingRegFile nullCapMemMetaVal
            else makeSimpleSIMTRegFile loadLatency (Just nullCapMemMetaVal)
        else makeNullSIMTRegFile

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
    scalarisableInstrCount :: Reg (Bit 32) <- makeReg 0

    -- Triggers from each lane to increment instruction count
    incInstrCountRegs :: [Reg (Bit 1)] <- replicateM SIMTLanes (makeDReg 0)

    -- Wire indicates that current instruction is scalarisable
    instrScalarisable5 <- makeDReg false
    instrScalarisable6 <- makeDReg false

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
          | (stateMem, pccMem) <- zip stateMems pccMems ]

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
        scalarisableInstrCount <== 0

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

            -- Incerement instructions that would be saved due to scalarisation
            when (c.useRegFileScalarisation && not enableCHERI) do
              when instrScalarisable6.val do
                scalarisableInstrCount <==
                  scalarisableInstrCount.val + instrInc
      else
        return ()

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
    warpStream <- makeGenericFairMergeTwo (makePipelineQueue 1) (const true)
                    (const true) (toStream releaseQueue, toStream warpQueue)

    always do
      -- Load state for next warp on each lane
      forM_ stateMems \stateMem -> do
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
            | (mem, i) <- zip stateMems [0..] ]

    -- Wait for leader index to be computed
    let states2_tmp =
          [iterateN (stage1Substages-1) buffer (mem.out) | mem <- stateMems]
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
      instrMem.load (toInstrAddr pc)

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
      regFile.loadA (warpId3.val, srcA instrMem.out)
      regFile.loadB (warpId3.val, srcB instrMem.out)

      -- Fetch capability meta-data from register file
      capRegFile.loadA (warpId3.val, srcA instrMem.out)
      capRegFile.loadB (warpId3.val, srcB instrMem.out)

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
      instr4 <== instrMem.out
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

    -- Count instruction executions that would be saved by scalarisation
    when (c.useRegFileScalarisation && not enableCHERI) do
      let isFieldInUse fld =
            case Map.lookup fld fieldMap4 of
              Nothing -> false
              Just opt -> opt.valid
      always do
        when (loadDelay (go4.val .&&. inv isSusp4.val)) do
          let scalarisable = andList
                [ isFieldInUse "rs1" ? (regFile.isScalarA, true)
                , isFieldInUse "rs2" ? (regFile.isScalarB, true) ]
          when scalarisable do instrScalarisable5 <== true

    -- Stages 5: Execute
    -- =================

    -- Insert warp id back into warp queue, except on warp command
    always do
      when go5 do
        instrScalarisable6 <== instrScalarisable5.val
        -- Reschedule warp if any thread suspended, or the instruction
        -- is not a warp command and an exception has not occurred
        if isSusp5 .||.
              (inv inputs.simtWarpCmdWire.active .&&. inv excGlobal.val)
          then do
            -- Reschedule
            dynamicAssert (warpQueue.notFull) "SIMT warp queue overflow"
            enq warpQueue warpId5
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
    let isSIMTPush = Map.findWithDefault false (c.simtPushTag) tagMap5
    let isSIMTPop = Map.findWithDefault false (c.simtPopTag) tagMap5

    -- Per-lane result wires
    resultWires :: [Wire (Bit 32)] <-
      replicateM SIMTLanes (makeWire dontCare)
    resultCapWires :: [Wire CapMemMeta] <-
      replicateM SIMTLanes (makeWire dontCare)

    -- Vector lane definition
    let makeLane makeExecStage threadActive suspMask regA regB regBorImm
                 capRegA capRegB stateMem incInstrCount pccMem
                 excLocal laneId resultWire resultCapWire = do

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
               <*> ZipList stateMems
               <*> ZipList incInstrCountRegs
               <*> ZipList pccMems
               <*> ZipList excLocals
               <*> ZipList [0..]
               <*> ZipList resultWires
               <*> ZipList resultCapWires

    -- Stage 6: Writeback
    -- ==================

    always do
      -- Process data from Execute stage
      let executeIdx = (old warpId5, old (dst instr5))
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
          instrMem.store (toInstrAddr req.simtReqAddr) (req.simtReqData)
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
                  , statId .==. simtStat_ScalarisableInstrs -->
                      scalarisableInstrCount.val ]
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
