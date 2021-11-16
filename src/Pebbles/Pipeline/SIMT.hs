module Pebbles.Pipeline.SIMT
  ( -- Pipeline configuration
    SIMTPipelineConfig(..)
    -- Pipeline inputs and outputs
  , SIMTPipelineIns(..)
  , SIMTPipelineOuts(..)
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
--  4. Operand Latch (consists of 1 or 2 sub-stages)
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

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.Stream
import Blarney.BitScan
import Blarney.QuadPortRAM
import Blarney.Interconnect

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
import Pebbles.CSRs.TrapCodes
import Pebbles.CSRs.Custom.SIMTDevice

-- CHERI imports
import CHERI.CapLib

-- | SIMT pipeline configuration
data SIMTPipelineConfig tag =
  SIMTPipelineConfig {
    instrMemInitFile :: Maybe String
    -- ^ Instruction memory initialisation file
  , instrMemLogNumInstrs :: Int
    -- ^ Instuction memory size (in number of instructions)
  , instrMemBase :: Integer
    -- ^ Base address of instruction memory in memory map
  , logNumWarps :: Int
    -- ^ Number of warps
  , logWarpSize :: Int
    -- ^ Number of threads per warp (number of lanes)
  , logMaxNestLevel :: Int
    -- ^ Number of bits used to track divergence nesting level
  , enableStatCounters :: Bool
    -- ^ Are stat counters enabled?
  , capRegInitFile :: Maybe String
    -- ^ File containing initial capability reg file meta-data
  , checkPCCFunc :: Maybe (Cap -> [(Bit 1, TrapCode)])
    -- ^ When CHERI is enabled, function to check PCC
  , enableCapRegFileTrace :: Bool
    -- ^ Trace accesses to capability register file
  , useExtraPreExecStage :: Bool
    -- ^ Extra pipeline stage?
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
  }

-- | SIMT pipeline inputs
data SIMTPipelineIns =
  SIMTPipelineIns {
      simtMgmtReqs :: Stream SIMTReq
      -- ^ Stream of pipeline management requests
    , simtWarpCmdWire :: Wire WarpCmd
      -- ^ When this wire is active, the warp currently in the execute
      -- stage (assumed to be converged) is issuing a warp command
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
  }

-- | Per-thread state
data SIMTThreadState t_logMaxNestLevel =
  SIMTThreadState {
    simtPC :: Bit 32
    -- ^ Program counter
  , simtNestLevel :: Bit t_logMaxNestLevel
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
  liftNat (c.logNumWarps) \(_ :: Proxy t_logWarps) ->
  liftNat (2 ^ c.logNumWarps) \(_ :: Proxy t_numWarps) ->
  liftNat (c.logWarpSize) \(_ :: Proxy t_logWarpSize) ->
  liftNat (2 ^ c.logWarpSize) \(_ :: Proxy t_warpSize) ->
  liftNat (c.instrMemLogNumInstrs) \(_ :: Proxy t_logInstrs) ->
  liftNat (c.logMaxNestLevel) \(_ :: Proxy t_logMaxNestLevel) -> do

    -- Sanity check
    staticAssert (c.logNumWarps <= valueOf @InstrIdWidth)
      "makeSIMTPipeline: WarpId is wider than InstrId"
    staticAssert (2 ^ c.logWarpSize == genericLength c.executeStage)
      "makeSIMTPipeline: warp size does not match number of execute units"

    -- Number of warps and warp size
    let numWarps = 2 ^ c.logNumWarps
    let warpSize = genericLength c.executeStage

    -- Is CHERI enabled?
    let enableCHERI = isJust c.checkPCCFunc

    -- Compute field selector functions from decode table
    let selMap = matchSel (c.decodeStage)

    -- Functions for extracting register ids from an instruction
    let srcA :: Instr -> RegId = getBitFieldSel selMap "rs1"
    let srcB :: Instr -> RegId = getBitFieldSel selMap "rs2"
    let dst  :: Instr -> RegId = getBitFieldSel selMap "rd"

    -- Queue of active warps
    warpQueue :: Queue (Bit t_logWarps) <- makeSizedQueue (c.logNumWarps)

    -- One block RAM of thread states per lane
    stateMems ::  [RAM (Bit t_logWarps)
                       (SIMTThreadState t_logMaxNestLevel) ] <-
      replicateM warpSize makeDualRAM

    -- One program counter capability RAM (meta-data only) per lane
    pccMems :: [RAM (Bit t_logWarps) CapMem] <-
      replicateM warpSize $
        if enableCHERI && not (c.useSharedPCC)
          then makeDualRAM
          else return nullRAM

    -- Instruction memory
    instrMem :: RAM (Bit t_logInstrs) Instr <-
      makeDualRAMCore (c.instrMemInitFile)

    -- Suspension bit for each thread
    suspBits :: [[Reg (Bit 1)]] <-
      replicateM warpSize (replicateM numWarps (makeReg false))

    -- Register files
    (regFilesA, regFilesB) ::
      ([RAM (Bit t_logWarps, RegId) (Bit 32)],
       [RAM (Bit t_logWarps, RegId) (Bit 32)]) <-
         unzip <$> replicateM warpSize makeQuadRAM

    -- Capability register files (meta-data only)
    (capRegFilesA, capRegFilesB) ::
      ([RAM (Bit t_logWarps, RegId) CapMemMeta],
       [RAM (Bit t_logWarps, RegId) CapMemMeta]) <-
         unzip <$> replicateM warpSize
           (if enableCHERI
              then makeQuadRAMCore (c.capRegInitFile)
              else return (nullRAM, nullRAM))

    -- Barrier bit for each warp
    barrierBits :: [Reg (Bit 1)] <- replicateM numWarps (makeReg 0)

    -- Count of number of warps in a barrier
    -- (Not read when all warps in barrier, so overflow not a problem)
    barrierCount :: Counter t_logWarps <- makeCounter dontCare

    -- Trigger for each stage
    go1 :: Reg (Bit 1) <- makeDReg false
    go3 :: Reg (Bit 1) <- makeDReg false
    go4 :: Reg (Bit 1) <- makeDReg false

    -- Warp id register, for each stage
    warpId1 :: Reg (Bit t_logWarps) <- makeReg dontCare
    warpId3 :: Reg (Bit t_logWarps) <- makeReg dontCare
    warpId4 :: Reg (Bit t_logWarps) <- makeReg dontCare

    -- Thread state, for each stage
    state3 :: Reg (SIMTThreadState t_logMaxNestLevel) <- makeReg dontCare
    state4 :: Reg (SIMTThreadState t_logMaxNestLevel) <- makeReg dontCare

    -- Active thread mask
    activeMask3 :: Reg (Bit t_warpSize) <- makeReg dontCare
    activeMask4 :: Reg (Bit t_warpSize) <- makeReg dontCare

    -- Instruction register for each stage
    instr4 :: Reg (Bit 32) <- makeReg dontCare

    -- Is any thread in the current warp suspended?
    isSusp4 :: Reg (Bit 1) <- makeReg dontCare

    -- Global exception register for entire core
    excGlobal :: Reg (Bit 1) <- makeReg false

    -- Program counter at point of exception
    excPC :: Reg (Bit 32) <- makeReg dontCare

    -- Per-lane exception register
    excLocals :: [Reg (Bit 1)] <- replicateM warpSize (makeReg false)

    -- Kernel response queue (indicates to CPU when kernel has finished)
    kernelRespQueue :: Queue SIMTResp <- makeShiftQueue 1

    -- Track how many warps have terminated
    completedWarps :: Reg (Bit t_logWarps) <- makeReg 0

    -- Track kernel success/failure
    kernelSuccess :: Reg (Bit 1) <- makeReg true

    -- Program counter capability registers
    pccShared :: Reg CapPipe <- makeReg dontCare
    pcc3 :: Reg Cap <- makeReg dontCare

    -- Stat counters
    cycleCount :: Reg (Bit 32) <- makeReg 0
    instrCount :: Reg (Bit 32) <- makeReg 0

    -- Triggers from each lane to increment instruction count
    incInstrCountRegs :: [Reg (Bit 1)] <- replicateM warpSize (makeDReg 0)

    -- Function to convert from 32-bit PC to instruction address
    let toInstrAddr :: Bit 32 -> Bit t_logInstrs =
          \pc -> truncateCast (slice @31 @2 pc)

    -- Pipeline Initialisation
    -- =======================

    -- Register to trigger pipeline initialisation at some PC
    startReg :: Reg (Option (Bit 32)) <- makeReg none

    -- Warp id counter, to initialise PC of each thread
    warpIdCounter :: Reg (Bit t_logWarps) <- makeReg 0

    -- Is the pipeline active?
    pipelineActive :: Reg (Bit 1) <- makeReg false

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
            pipelineActive <== true
            warpIdCounter <== 0
            cycleCount <== 0
            instrCount <== 0
          else
            warpIdCounter <== warpIdCounter.val + 1

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

    -- Stage 0: Warp Scheduling
    -- ========================

    -- Queue of warps that have left a barrier (half throughput)
    releaseQueue :: Queue (Bit t_logWarps) <-
      makeSizedQueueConfig
        SizedQueueConfig {
          sizedQueueLogSize = c.logNumWarps
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
              , fromInteger i :: Bit t_logWarpSize )
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
      let activeMask :: Bit t_warpSize = fromBitList activeList
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
      -- Fetch operands from each register file
      forM_ (zip regFilesA regFilesB) \(rfA, rfB) -> do
        rfA.load (warpId3.val, srcA instrMem.out)
        rfB.load (warpId3.val, srcB instrMem.out)

      -- Fetch capability meta-data from each register file
      forM_ (zip capRegFilesA capRegFilesB) \(rfA, rfB) -> do
        rfA.load (warpId3.val, srcA instrMem.out)
        rfB.load (warpId3.val, srcB instrMem.out)

      -- Is any thread in warp suspended?
      -- (In future, consider only suspension bits of active threads)
      let isSusp3 = orList [map (.val) regs ! warpId3.val | regs <- suspBits]
      isSusp4 <== isSusp3

      -- Capability register file tracing
      case c.checkPCCFunc of
        Nothing -> return ()
        Just _ ->
          if c.enableCapRegFileTrace
            then do
              let instr = instrMem.out
              let (_, fieldMap) = matchMap False (c.decodeStage) instr
              let useRegA = isSome (getBitField fieldMap "rs1" :: Option RegId)
              let useRegB = isSome (getBitField fieldMap "rs2" :: Option RegId)
              when (go3.val .&&. inv isSusp3) do
                display "[CapRegFileTrace] read"
                        " time=" (cycleCount.val)
                        " pc=0x" (formatHex 0 $ state3.val.simtPC)
                        " warp=" (warpId3.val)
                        " active=0x" (formatHex 0 $ activeMask3.val)
                        " rs1=" (if useRegA then srcA instr else 0)
                        " rs2=" (if useRegB then srcB instr else 0)
            else return ()

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

    -- This stage may use one or two sub-stages
    let extraReg :: Bits a => a -> a
        extraReg inp = if c.useExtraPreExecStage then old inp else inp

    -- Decode instruction
    let (tagMap4, fieldMap4) =
          matchMap False (c.decodeStage) (extraReg instr4.val)

    -- Get stage 5 register value
    let getReg5 regFile = extraReg (old regFile.out)

    -- Get stage 5 capability register value
    let getCapReg5 regFile capRegFile = old $ decodeCapMem $ extraReg $
          (capRegFile.out # regFile.out)

    -- Get stage 5 register B or immediate
    let getRegBorImm5 rf = old $
          if Map.member "imm" fieldMap4
            then let imm = getBitField fieldMap4 "imm"
                 in  imm.valid ? (imm.val, extraReg rf.out)
            else extraReg rf.out

    -- Propagate signals to stage 5
    let pcc5 = extraReg (old pcc4)
    let isSusp5 = extraReg (old isSusp4.val)
    let warpId5 = extraReg (old warpId4.val)
    let activeMask5 = extraReg (old activeMask4.val)
    let instr5 = extraReg (old instr4.val)
    let state5 = extraReg (old state4.val)
    let go5 = if c.useExtraPreExecStage
                then delay false $ delay false (go4.val)
                else delay false (go4.val)

    -- Buffer the decode tables
    let tagMap5 = Map.map old tagMap4

    -- Stages 5 and 6: Execute and Writeback
    -- =====================================

    -- Insert warp id back into warp queue, except on warp command
    always do
      when go5 do
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

    -- Vector lane definition
    let makeLane makeExecStage threadActive suspMask regFileA
                 regFileB capRegFileA capRegFileB
                 stateMem incInstrCount pccMem excLocal laneId = do

          -- Per lane interfacing
          pcNextWire :: Wire (Bit 32) <-
            makeWire (state5.simtPC + 4)
          pccNextWire :: Wire CapPipe <- makeWire dontCare
          retryWire  :: Wire (Bit 1) <- makeWire false
          suspWire   :: Wire (Bit 1) <- makeWire false
          resultWire :: Wire (Bit 32) <- makeWire dontCare
          resultCapWire :: Wire CapMemMeta <- makeWire dontCare

          -- Register operands
          let regA = getReg5 regFileA
          let regB = getReg5 regFileB
          let capRegA = getCapReg5 regFileA capRegFileA
          let capRegB = getCapReg5 regFileB capRegFileB
          let regBorImm = getRegBorImm5 regFileB

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
            , suspend = do
                suspWire <== true
                return
                  InstrInfo {
                    instrId = zeroExtendCast warpId5
                  , instrDest = dst instr5
                  }
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
              when (suspWire.val) do
                suspMask!warpId5 <== true

            -- Writeback stage
            when (inv excLocal.val) do
              let idx = (old warpId5, old (dst instr5))
              when (delay false (resultWire.active)) do
                regFileA.store idx (old resultWire.val)
              if enableCHERI
                then do
                  when (delay false (resultCapWire.active)) do
                    capRegFileA.store idx (old resultCapWire.val)
                    if c.enableCapRegFileTrace
                      then do
                        let cap = fromMem $ unpack
                                    (old resultCapWire.val #
                                       old resultWire.val)
                        display "[CapRegFileTrace] write"
                                " time="  (cycleCount.val)
                                " lane="  (show laneId)
                                " warp="  (fst idx)
                                " rd="    (snd idx)
                                " valid=" (isValidCap cap)
                                " base=0x" (formatHex 0 $ getBase cap)
                                " top=0x"  (formatHex 0 $ getTop cap)
                                " meta=0x" (formatHex 0 $ getMeta cap)
                                " addr=0x" (formatHex 0 $ getAddr cap)
                      else return ()
                else return ()

            -- Thread resumption
            when (execStage.resumeReqs.canPeek) do
              let req = execStage.resumeReqs.peek
              let idx = (truncateCast req.resumeReqInfo.instrId,
                         req.resumeReqInfo.instrDest)
              when (req.resumeReqInfo.instrDest .!=. 0 .&&.
                      inv excGlobal.val) do
                regFileB.store idx (req.resumeReqData)
                if enableCHERI
                  then do
                    let capVal = isSome req.resumeReqCap ?
                          (req.resumeReqCap.val, nullCapMemMetaVal)
                    capRegFileB.store idx capVal
                    if c.enableCapRegFileTrace
                      then do
                        let cap = fromMem $ unpack
                                    (capVal # req.resumeReqData)
                        display "[CapRegFileTrace] resume"
                                " time="  (cycleCount.val)
                                " lane="  (show laneId)
                                " warp="  (fst idx)
                                " rd="    (snd idx)
                                " valid=" (isValidCap cap)
                                " base=0x" (formatHex 0 $ getBase cap)
                                " top=0x"  (formatHex 0 $ getTop cap)
                                " meta=0x" (formatHex 0 $ getMeta cap)
                                " addr=0x" (formatHex 0 $ getAddr cap)
                      else return ()
                  else return ()
              suspMask!(req.resumeReqInfo.instrId) <== false
              execStage.resumeReqs.consume

    -- Create vector lanes
    sequence $ getZipList $
      makeLane <$> ZipList (c.executeStage)
               <*> ZipList (toBitList activeMask5)
               <*> ZipList suspBits
               <*> ZipList regFilesA
               <*> ZipList regFilesB
               <*> ZipList capRegFilesA
               <*> ZipList capRegFilesB
               <*> ZipList stateMems
               <*> ZipList incInstrCountRegs
               <*> ZipList pccMems
               <*> ZipList excLocals
               <*> ZipList [0..]

    -- Handle barrier release
    -- ======================

    -- Warps per block (a block is a group of threads that synchronise
    -- on a barrier). A value of zero indicates all warps.
    warpsPerBlock :: Reg (Bit t_logWarps) <- makeReg 0

    -- Mask that identifies a block of threads
    barrierMask :: Reg (Bit t_numWarps) <- makeReg ones

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
          let n :: Bit t_logWarps = truncateCast req.simtReqData
          warpsPerBlock <== n
          barrierMask <== n .==. 0 ? (ones, (1 .<<. n) - 1)
          inputs.simtMgmtReqs.consume
        -- Read stat counter
        when (req.simtReqCmd .==. simtCmd_AskStats) do
          let getStatId req = unpack (truncate req.simtReqAddr)
          when (inv pipelineActive.val .&&. kernelRespQueue.notFull) do
            let resp = (getStatId req .==. simtStat_Cycles) ?
                         (cycleCount.val, instrCount.val)
            enq kernelRespQueue resp
            inputs.simtMgmtReqs.consume

    -- Pipeline outputs
    return
      SIMTPipelineOuts {
        simtMgmtResps = toStream kernelRespQueue
      , simtCurrentWarpId = zeroExtendCast warpId5
      , simtKernelAddr = kernelAddrReg.val
      }

-- Barrier release unit
-- ====================

-- This logic looks for threads in a block that have all entered a
-- barrier, and then releases them.

-- | Inputs to barrier release unit
data BarrierReleaseIns t_logWarps t_numWarps =
  BarrierReleaseIns {
    relWarpsPerBlock :: Bit t_logWarps
    -- ^ Number of warps per block of synchronising threads
  , relBarrierMask :: Bit t_numWarps
    -- ^ Mask that identifies a block of threads
  , relBarrierVec :: Bit t_numWarps
    -- ^ Bit vector denoting warps currently in barrier
  , relAction :: Bit t_logWarps -> Action ()
    -- ^ Action to perform on a release
  }

-- | Barrier release unit
makeBarrierReleaseUnit ::
  (KnownNat t_logWarps, KnownNat t_numWarps) =>
    BarrierReleaseIns t_logWarps t_numWarps -> Module ()
makeBarrierReleaseUnit ins = do
  -- Mask that identifies a block of threads
  barrierMask :: Reg (Bit t_numWarps) <- makeReg ones

  -- Shift register for barrier release logic
  barrierShiftReg :: Reg (Bit t_numWarps) <- makeReg dontCare

  -- For release state machine
  releaseState :: Reg (Bit 2) <- makeReg 0

  -- Warp counters for release logic
  releaseWarpId :: Reg (Bit t_logWarps) <- makeReg dontCare
  releaseWarpCount :: Reg (Bit t_logWarps) <- makeReg dontCare

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
