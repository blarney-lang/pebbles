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
  , logMaxNestLevel :: Int
    -- ^ Number of bits used to track divergence nesting level
  , enableStatCounters :: Bool
    -- ^ Are stat counters enabled?
  , capRegInitFile :: Maybe String
    -- ^ File containing initial capability reg file meta-data
  , checkPCCFunc :: Maybe (InternalCap -> [(Bit 1, TrapCode)])
    -- ^ When CHERI is enabled, function to check PCC
  , useExtraPreExecStage :: Bool
    -- ^ Extra pipeline stage?
  , decodeStage :: [(String, tag)]
    -- ^ Decode table
  , executeStage :: [State -> Module ExecuteStage]
    -- ^ List of execute stages, one per lane
    -- The size of this list is the warp size
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
data SIMTThreadState t_logInstrs t_logMaxNestLevel =
  SIMTThreadState {
    simtPC :: Bit t_logInstrs
    -- ^ Program counter
  , simtNestLevel :: Bit t_logMaxNestLevel
    -- ^ SIMT divergence nesting level
  , simtRetry :: Bit 1
    -- ^ The last thing this thread did was a retry
  }
  deriving (Generic, Bits)

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
  liftNat (c.executeStage.length) \(_ :: Proxy t_warpSize) ->
  liftNat (c.instrMemLogNumInstrs) \(_ :: Proxy t_logInstrs) ->
  liftNat (c.logMaxNestLevel) \(_ :: Proxy t_logMaxNestLevel) -> do

    -- Sanity check
    staticAssert (c.logNumWarps <= valueOf @InstrIdWidth)
      "makeSIMTPipeline: WarpId is wider than InstrId"

    -- Number of warps and warp size
    let numWarps = 2 ^ c.logNumWarps
    let warpSize = c.executeStage.length

    -- Is CHERI enabled?
    let enableCHERI = c.checkPCCFunc.isJust

    -- Compute field selector functions from decode table
    let selMap = matchSel (c.decodeStage)

    -- Functions for extracting register ids from an instruction
    let srcA :: Instr -> RegId = getFieldSel selMap "rs1"
    let srcB :: Instr -> RegId = getFieldSel selMap "rs2"
    let dst  :: Instr -> RegId = getFieldSel selMap "rd"

    -- Queue of active warps
    warpQueue :: Queue (Bit t_logWarps) <- makeSizedQueue (c.logNumWarps)

    -- One block RAM of thread states per lane
    stateMems ::  [RAM (Bit t_logWarps)
                       (SIMTThreadState t_logInstrs t_logMaxNestLevel) ] <-
      replicateM warpSize makeDualRAM

    -- One program counter capability RAM (meta-data only) per lane
    pccMems :: [RAM (Bit t_logWarps) InternalCapMetaData] <-
      replicateM warpSize $
        if enableCHERI
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
      ([RAM (Bit t_logWarps, RegId) InternalCapMetaData],
       [RAM (Bit t_logWarps, RegId) InternalCapMetaData]) <-
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
    state3 :: Reg (SIMTThreadState t_logInstrs t_logMaxNestLevel) <-
      makeReg dontCare
    state4 :: Reg (SIMTThreadState t_logInstrs t_logMaxNestLevel) <-
      makeReg dontCare

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

    -- Stat counters
    cycleCount :: Reg (Bit 32) <- makeReg 0
    instrCount :: Reg (Bit 32) <- makeReg 0

    -- Triggers from each lane to increment instruction count
    incInstrCountRegs :: [Reg (Bit 1)] <- replicateM warpSize (makeDReg 0)

    -- Pipeline Initialisation
    -- =======================

    -- Register to trigger pipeline initialisation at some PC
    startReg :: Reg (Option (Bit t_logInstrs)) <- makeReg none

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
        sequence_
          [ do store stateMem (warpIdCounter.val) initState
               if enableCHERI
                 then do
                   -- TODO: constrain initial PCCs
                   let initPCC = almightyCapMetaVal
                   store pccMem (warpIdCounter.val) initPCC
                 else return ()
          | (stateMem, pccMem) <- zip stateMems pccMems ]

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
                  map zeroExtend (map val incInstrCountRegs)
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
        load stateMem (warpStream.peek)

      -- Load PCC for next warp on each lane
      if enableCHERI
        then do
          forM_ pccMems \pccMem -> do
            load pccMem (warpStream.peek)
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
    let maxOf a b =
          if (a.simtNestLevel # a.simtRetry) .>.
             (b.simtNestLevel # b.simtRetry) then a else b
    let state2 = pipelinedTree1 stage1Substages maxOf
                   [mem.out | mem <- stateMems]

    -- Latch PCCs
    let pcs2 = [ iterateN stage1Substages (delay dontCare) (mem.out.simtPC)
               | mem <- stateMems]
    let pccs2 = [ iterateN stage1Substages (delay dontCare) (mem.out)
                | mem <- pccMems]

    -- Trigger stage 2
    let stateMemOuts2 =
          [iterateN stage1Substages buffer (mem.out) | mem <- stateMems]
    let warpId2 = iterateN stage1Substages buffer (warpId1.val)
    let go2 = iterateN stage1Substages (delay 0) (go1.val)

    -- Stage 2: Instruction Fetch
    -- ==========================

    -- Functions to convert between 32-bit PC and instruction address
    let fromPC :: Bit 32 -> Bit t_logInstrs =
          \pc -> truncateCast (slice @31 @2 pc)
    let toPC :: Bit t_logInstrs -> Bit 32 =
          \addr -> fromInteger (c.instrMemBase) .|.
                     zeroExtendCast addr # (0 :: Bit 2)

    -- Set address of for each PCC
    let pccs3 =
          [ let pccCap = unsplitCap (pcc, toPC 0) in
              delay dontCare $ setAddr pccCap (toPC pc)
          | (pc, pcc) <- zip pcs2 pccs2]

    always do
      -- Compute active thread mask
      let activeList = [out === state2 | out <- stateMemOuts2]
      let activeMask :: Bit t_warpSize = fromBitList activeList
      activeMask3 <== activeMask

      -- Assert that at least one thread in the warp must be active
      when go2 do
        dynamicAssert (activeList.orList)
          "SIMT pipeline error: no active threads in warp"

      -- Issue load to instruction memory
      load instrMem (state2.simtPC)

      -- Trigger stage 3
      warpId3 <== warpId2
      state3 <== state2
      go3 <== go2

    -- Stage 3: Operand Fetch
    -- ======================

    let pccs4 = [delay dontCare (pcc.value) | pcc <- pccs3]

    always do
      -- Fetch operands from each register file
      forM_ (zip regFilesA regFilesB) \(rfA, rfB) -> do
        load rfA (warpId3.val, instrMem.out.srcA)
        load rfB (warpId3.val, instrMem.out.srcB)

      -- Fetch capability meta-data from each register file
      forM_ (zip capRegFilesA capRegFilesB) \(rfA, rfB) -> do
        load rfA (warpId3.val, instrMem.out.srcA)
        load rfB (warpId3.val, instrMem.out.srcB)

      -- Is any thread in warp suspended?
      -- (In future, consider only suspension bits of active threads)
      isSusp4 <== orList [map val regs ! warpId3.val | regs <- suspBits]

      -- Check PCC
      case c.checkPCCFunc of
        Nothing -> return ()
        Just checkPCC -> sequence_
          [ do let table = checkPCC (pcc.value)
               let exception = orList [cond | (cond, _) <- table]
               let ok = inv active .||. pcc.exact .&&. inv exception
               when (go3.val) do
                 when (inv ok) do
                   excLocal <== true
                   let trapCode = priorityIf table (excCapCode 0)
                   display "SIMT pipeline: PCC exception: "
                           " code=" trapCode
          | (pcc, active, excLocal) <-
              zip3 pccs3 (activeMask3.val.toBitList) excLocals ]

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
          matchMap False (c.decodeStage) (instr4.val.extraReg)

    -- Get stage 5 register value
    let getReg5 regFile = regFile.out.old.extraReg

    -- Get stage 5 capability register value
    let getCapReg5 regFile capRegFile = old $ decodeCap $ extraReg $
          unsplitCap (capRegFile.out, regFile.out)

    -- Get stage 5 register B or immediate
    let getRegBorImm5 rf = old $
          if Map.member "imm" fieldMap4
            then let imm = getField fieldMap4 "imm"
                 in  imm.valid ? (imm.val, rf.out.extraReg)
            else rf.out.extraReg

    -- Propagate signals to stage 5
    let pccs5 = map extraReg (map old pccs4)
    let isSusp5 = isSusp4.val.old.extraReg
    let warpId5 = warpId4.val.old.extraReg
    let activeMask5 = activeMask4.val.old.extraReg
    let instr5 = instr4.val.old.extraReg
    let state5 = state4.val.old.extraReg
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
              (inputs.simtWarpCmdWire.active.inv .&&. excGlobal.val.inv)
          then do
            -- Reschedule
            dynamicAssert (warpQueue.notFull) "SIMT warp queue overflow"
            enq warpQueue warpId5
          else do
            -- Instruction is warp command or exception has occurred.
            -- Warp commands assume that warp has converged
            dynamicAssert (excGlobal.val.inv .==>. activeMask5 .==. ones)
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
      when (orList $ map val excLocals) do
        excGlobal <== true
        -- Track exception PC
        excPC <== state5.simtPC.toPC.old

    -- Is it a SIMT convergence instruction?
    let isSIMTPush = Map.findWithDefault false (c.simtPushTag) tagMap5
    let isSIMTPop = Map.findWithDefault false (c.simtPopTag) tagMap5

    -- Vector lane definition
    let makeLane makeExecStage threadActive suspMask regFileA
                 regFileB capRegFileA capRegFileB
                 stateMem incInstrCount pcc pccMem excLocal = do

          -- Per lane interfacing
          pcNextWire :: Wire (Bit t_logInstrs) <-
            makeWire (state5.simtPC + 1)
          pccNextWire :: Wire InternalCapMetaData <- makeWire dontCare
          retryWire  :: Wire (Bit 1) <- makeWire false
          suspWire   :: Wire (Bit 1) <- makeWire false
          resultWire :: Wire (Bit 32) <- makeWire dontCare
          resultCapWire :: Wire InternalCapMetaData <- makeWire dontCare

          -- Register operands
          let regA = getReg5 regFileA
          let regB = getReg5 regFileB
          let capRegA = getCapReg5 regFileA capRegFileA
          let capRegB = getCapReg5 regFileB capRegFileB
          let regBorImm = getRegBorImm5 regFileB

          -- Is destination register non-zero?
          let destNonZero = instr5.dst .!=. 0

          -- Instantiate execute stage
          execStage <- makeExecStage
            State {
              instr = instr5
            , opA = regA
            , opB = regB
            , capA = capRegA
            , capB = capRegB
            , opBorImm = regBorImm
            , opAIndex = instr5.srcA
            , opBIndex = instr5.srcB
            , resultIndex = instr5.dst
            , pc = ReadWrite (state5.simtPC.toPC) \writeVal -> do
                     pcNextWire <== writeVal.fromPC
            , pcc = ReadWrite pcc \cap -> do
                      if enableCHERI
                        then do
                          let (meta, addr) = splitCap cap
                          pcNextWire <== fromPC addr
                          pccNextWire <== meta
                        else return ()
            , result = WriteOnly \writeVal ->
                         when destNonZero do
                           resultWire <== writeVal
                           if enableCHERI
                             then resultCapWire <== nullCapMetaVal
                             else return ()
            , resultCap = WriteOnly \cap ->
                            when destNonZero do
                              let (meta, addr) = splitCap cap
                              resultWire <== addr
                              resultCapWire <== meta
            , suspend = do
                suspWire <== true
                return
                  InstrInfo {
                    instrId = warpId5.zeroExtendCast
                  , instrDest = instr5.dst
                  , instrTagMask = false
                  }
            , retry = retryWire <== true
            , opcode = packTagMap tagMap5
            , trap = \code -> do
                excLocal <== true
                display "SIMT exception occurred: " code
                        " pc=0x" (formatHex 8 (state5.simtPC.toPC))
            }

          always do
            -- Execute stage
            when (go5 .&&. threadActive .&&.
                    isSusp5.inv .&&. excGlobal.val.inv) do
              -- Trigger execute stage
              execStage.execute

              -- Update thread state
              let nestInc = isSIMTPush.zeroExtendCast
              let nestDec = isSIMTPop.zeroExtendCast
              let nestLevel = state5.simtNestLevel
              dynamicAssert (isSIMTPop .==>. nestLevel .!=. 0)
                  "SIMT pipeliene: SIMT nest level underflow"
              dynamicAssert (isSIMTPush .==>. nestLevel .!=. ones)
                  "SIMT pipeliene: SIMT nest level overflow"
              store stateMem warpId5
                SIMTThreadState {
                    -- Only update PC if not retrying
                    simtPC = retryWire.val ?
                      (state5.simtPC, pcNextWire.val)
                  , simtNestLevel = (nestLevel + nestInc) - nestDec
                  , simtRetry = retryWire.val
                  }
              when (pccNextWire.active) do
                store pccMem warpId5 (pccNextWire.val)

              -- Increment instruction count
              when (retryWire.val.inv) do
                incInstrCount <== true

              -- Update suspension bits
              when (suspWire.val) do
                suspMask!warpId5 <== true

            -- Writeback stage
            when (excLocal.val.inv) do
              let idx = (warpId5.old, instr5.dst.old)
              when (delay false (resultWire.active)) do
                store regFileA idx (resultWire.val.old)
              if enableCHERI
                then do
                  when (delay false (resultCapWire.active)) do
                    store capRegFileA idx (resultCapWire.val.old)
                else return ()

            -- Thread resumption
            when (execStage.resumeReqs.canPeek) do
              let req = execStage.resumeReqs.peek
              let idx = (req.resumeReqInfo.instrId.truncateCast,
                         req.resumeReqInfo.instrDest)
              when (req.resumeReqInfo.instrDest .!=. 0 .&&.
                      excGlobal.val.inv) do
                store regFileB idx (req.resumeReqData)
                if enableCHERI
                  then do
                    when (req.resumeReqCap.isSome) do
                      store capRegFileB idx (req.resumeReqCap.val)
                  else return ()
              suspMask!(req.resumeReqInfo.instrId) <== false
              execStage.resumeReqs.consume

    -- Create vector lanes
    sequence $ getZipList $
      makeLane <$> ZipList (c.executeStage)
               <*> ZipList (activeMask5.toBitList)
               <*> ZipList suspBits
               <*> ZipList regFilesA
               <*> ZipList regFilesB
               <*> ZipList capRegFilesA
               <*> ZipList capRegFilesB
               <*> ZipList stateMems
               <*> ZipList incInstrCountRegs
               <*> ZipList pccs5
               <*> ZipList pccMems
               <*> ZipList excLocals

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
      , relBarrierVec = fromBitList (map val barrierBits)
      , relAction = \warpId -> do
          when (excGlobal.val.inv) do
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
          dynamicAssert (busy.inv)
            "SIMT pipeline: writing instruction while pipeline busy"
          store instrMem (req.simtReqAddr.fromPC) (req.simtReqData)
          inputs.simtMgmtReqs.consume
        -- Start pipeline
        when (req.simtReqCmd .==. simtCmd_StartPipeline) do
          when (busy.inv) do
            startReg <== some (req.simtReqAddr.fromPC)
            kernelAddrReg <== req.simtReqData
            inputs.simtMgmtReqs.consume
        -- Set warps per block
        when (req.simtReqCmd .==. simtCmd_SetWarpsPerBlock) do
          dynamicAssert (busy.inv)
            "SIMT pipeline: setting warps per block while pipeline busy"
          let n :: Bit t_logWarps = req.simtReqData.truncateCast
          warpsPerBlock <== n
          barrierMask <== n .==. 0 ? (ones, (1 .<<. n) - 1)
          inputs.simtMgmtReqs.consume
        -- Read stat counter
        when (req.simtReqCmd .==. simtCmd_AskStats) do
          let getStatId req = req.simtReqAddr.truncate.unpack
          when (pipelineActive.val.inv .&&. kernelRespQueue.notFull) do
            let resp = (req.getStatId .==. simtStat_Cycles) ?
                         (cycleCount.val, instrCount.val)
            enq kernelRespQueue resp
            inputs.simtMgmtReqs.consume

    -- Pipeline outputs
    return
      SIMTPipelineOuts {
        simtMgmtResps = kernelRespQueue.toStream
      , simtCurrentWarpId = warpId5.zeroExtendCast
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
