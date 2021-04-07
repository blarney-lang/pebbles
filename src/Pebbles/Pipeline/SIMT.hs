module Pebbles.Pipeline.SIMT
  ( -- Pipeline configuration
    SIMTPipelineConfig(..)
    -- Pipeline inputs and outputs
  , SIMTPipelineIns(..)
  , SIMTPipelineOuts(..)
    -- Pipeline module
  , makeSIMTPipeline
  ) where

-- Simple 32-bit SIMT pipeline, with implicit thread convergence (a la
-- Simty) and a configurable number of warps and warp size.
--
-- There are 9 pipeline stages:
--
--  0. Warp Scheduling
--  1. Active Thread Selection (consists of 3 sub-stages)
--  2. Instruction Fetch
--  3. Operand Fetch
--  4. Operand Latch
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
import Blarney.Interconnect

-- General imports
import Data.List
import Data.Proxy
import qualified Data.Map as Map
import Control.Applicative hiding (some)

-- Pebbles imports
import Pebbles.Util.List
import Pebbles.Pipeline.Interface
import Pebbles.CSRs.Custom.SIMTDevice
import Pebbles.Pipeline.SIMT.Management

-- | SIMT pipeline configuration
data SIMTPipelineConfig tag =
  SIMTPipelineConfig {
    -- | Instruction memory initialisation file
    instrMemInitFile :: Maybe String
    -- | Instuction memory size (in number of instructions)
  , instrMemLogNumInstrs :: Int
    -- | Number of warps
  , logNumWarps :: Int
    -- | Number of bits used to track function call depth
  , logMaxCallDepth :: Int
    -- | Decode table
  , decodeStage :: [(String, tag)]
    -- | List of execute stages, one per lane
    -- The size of this list is the warp size
  , executeStage :: [State -> Module ExecuteStage]
  }

-- | SIMT pipeline inputs
data SIMTPipelineIns =
  SIMTPipelineIns {
      -- | Stream of pipeline management requests
      simtMgmtReqs :: Stream SIMTReq
      -- | When this wire is active, the warp currently in the execute
      -- stage (assumed to be converged) is issuing a warp command
    , simtWarpCmdWire :: Wire WarpCmd
      -- | A pulse on these wires indicates that current instruction (in
      -- execute) is incrementing/decrementing the function call
      -- depth; one bit per lane
    , simtIncCallDepth :: [Bit 1]
    , simtDecCallDepth :: [Bit 1]
  }

-- | SIMT pipeline outputs
data SIMTPipelineOuts =
  SIMTPipelineOuts {
      -- | Stream of pipeline management responses
      simtMgmtResps :: Stream SIMTResp
      -- | Warp id of instruction currently in execute stage
    , simtCurrentWarpId :: Bit 32
      -- | Address of kernel closure as set by CPU
    , simtKernelAddr :: Bit 32
  }

-- | Per-thread state
data SIMTThreadState t_logInstrs t_logMaxCallDepth =
  SIMTThreadState {
    -- | Program counter
    simtPC :: Bit t_logInstrs
    -- | Function call depth (smaller is deeper)
  , simtCallDepth :: Bit t_logMaxCallDepth
  }
  deriving (Generic, Bits)

-- | SIMT pipeline module
makeSIMTPipeline :: Tag tag =>
     -- | SIMT configuration options
     SIMTPipelineConfig tag
     -- | SIMT pipeline inputs
  -> SIMTPipelineIns
     -- | SIMT pipeline outputs
  -> Module SIMTPipelineOuts
makeSIMTPipeline c inputs =
  -- Lift some parameters to the type level
  liftNat (c.logNumWarps) \(_ :: Proxy t_logWarps) ->
  liftNat (2 ^ c.logNumWarps) \(_ :: Proxy t_numWarps) ->
  liftNat (c.executeStage.length) \(_ :: Proxy t_warpSize) ->
  liftNat (c.instrMemLogNumInstrs) \(_ :: Proxy t_logInstrs) ->
  liftNat (c.logMaxCallDepth) \(_ :: Proxy t_logMaxCallDepth) -> do

    -- Sanity check
    staticAssert (c.logNumWarps <= valueOf @InstrIdWidth)
      "makeSIMTPipeline: WarpId is wider than InstrId"

    -- Number of warps and warp size
    let numWarps = 2 ^ c.logNumWarps
    let warpSize = c.executeStage.length

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
                       (SIMTThreadState t_logInstrs t_logMaxCallDepth) ] <-
      replicateM warpSize makeDualRAM

    -- Instruction memory
    instrMem :: RAM (Bit t_logInstrs) Instr <-
      makeDualRAMCore (c.instrMemInitFile)

    -- Suspension bit for each thread
    suspBits :: [[Reg (Bit 1)]] <-
      replicateM warpSize (replicateM numWarps (makeReg false))

    -- Register files
    regFilesA :: [RAM (Bit t_logWarps, RegId) (Bit 32)] <-
      replicateM warpSize makeDualRAM
    regFilesB :: [RAM (Bit t_logWarps, RegId) (Bit 32)] <-
      replicateM warpSize makeDualRAM

    -- Barrier bit for each warp
    barrierBits :: [Reg (Bit 1)] <- replicateM numWarps (makeReg 0)

    -- Trigger for each stage
    go1 :: Reg (Bit 1) <- makeDReg false
    go3 :: Reg (Bit 1) <- makeDReg false
    go4 :: Reg (Bit 1) <- makeDReg false
    go5 :: Reg (Bit 1) <- makeDReg false

    -- Warp id register, for each stage
    warpId1 :: Reg (Bit t_logWarps) <- makeReg dontCare
    warpId3 :: Reg (Bit t_logWarps) <- makeReg dontCare
    warpId4 :: Reg (Bit t_logWarps) <- makeReg dontCare
    warpId5 :: Reg (Bit t_logWarps) <- makeReg dontCare

    -- Call depth register, for each stage
    state2 :: Reg (SIMTThreadState t_logInstrs t_logMaxCallDepth) <-
      makeReg dontCare
    state3 :: Reg (SIMTThreadState t_logInstrs t_logMaxCallDepth) <-
      makeReg dontCare
    state4 :: Reg (SIMTThreadState t_logInstrs t_logMaxCallDepth) <-
      makeReg dontCare
    state5 :: Reg (SIMTThreadState t_logInstrs t_logMaxCallDepth) <-
      makeReg dontCare

    -- Active thread mask
    activeMask3 :: Reg (Bit t_warpSize) <- makeReg dontCare
    activeMask4 :: Reg (Bit t_warpSize) <- makeReg dontCare
    activeMask5 :: Reg (Bit t_warpSize) <- makeReg dontCare

    -- Instruction register for each stage
    instr4 :: Reg (Bit 32) <- makeReg dontCare
    instr5 :: Reg (Bit 32) <- makeReg dontCare

    -- Is any thread in the current warp suspended?
    isSusp4 :: Reg (Bit 1) <- makeReg dontCare
    isSusp5 :: Reg (Bit 1) <- makeReg dontCare

    -- Kernel response queue (indicates to CPU when kernel has finished)
    kernelRespQueue :: Queue SIMTResp <- makeShiftQueue 1

    -- Track how many warps have terminated
    completedWarps :: Reg (Bit t_logWarps) <- makeReg 0

    -- Track kernel success/failure
    kernelSuccess :: Reg (Bit 1) <- makeReg true

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
              , simtCallDepth = ones
              }
        sequence_ [ store stateMem (warpIdCounter.val) initState
                  | stateMem <- stateMems ]

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
          else
            warpIdCounter <== warpIdCounter.val + 1

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
                    (toStream releaseQueue, toStream warpQueue)

    always do
      -- Load state for next warp on each lane
      forM_ stateMems \stateMem -> do
        load stateMem (warpStream.peek)

      -- Buffer warp id for stage 1
      warpId1 <== warpStream.peek

      -- Trigger stage 1
      when (warpStream.canPeek .&&. pipelineActive.val) do
        warpStream.consume
        go1 <== true

    -- Stage 1: Active Thread Selection
    -- ================================

    -- For timing, we split this stage over several cycles
    let stage1Substages = 3

    -- Active threads are those with the max call depth,
    -- and on a tie, the min PC
    let packState s = s.simtCallDepth # s.simtPC
    let minOf a b = if packState a .<. packState b then a else b
    let state2 = pipelinedTree1 stage1Substages minOf
                   [mem.out | mem <- stateMems]

    -- Trigger stage 2
    let stateMemOuts2 =
          [iterateN stage1Substages buffer (mem.out) | mem <- stateMems]
    let warpId2 = iterateN stage1Substages buffer (warpId1.val)
    let go2 = iterateN stage1Substages (delay 0) (go1.val)

    -- Stage 2: Instruction Fetch
    -- ==========================

    always do
      -- Compute active thread mask
      let activeList = [out === state2 | out <- stateMemOuts2]
      let activeMask :: Bit t_warpSize = fromBitList activeList
      activeMask3 <== activeMask

      -- Assert that at least one thread in the warp must be active
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

    always do
      -- Fetch operands from each register file
      forM_ (zip regFilesA regFilesB) \(rfA, rfB) -> do
        load rfA (warpId3.val, instrMem.out.srcA)
        load rfB (warpId3.val, instrMem.out.srcB)

      -- Is any thread in warp suspended?
      -- (In future, consider only suspension bits of active threads)
      isSusp4 <== orList [map val regs ! warpId3.val | regs <- suspBits]

      -- Trigger stage 4
      warpId4 <== warpId3.val
      activeMask4 <== activeMask3.val
      instr4 <== instrMem.out
      state4 <== state3.val
      go4 <== go3.val

    -- Stage 4: Operand Latch
    -- ======================

    -- Decode instruction
    let (tagMap4, fieldMap4) = matchMap False (c.decodeStage) (instr4.val)

    -- Choose register B or immediate
    let getRegBOrImm regB =
          if Map.member "imm" fieldMap4
            then let imm = getField fieldMap4 "imm"
                 in  imm.valid ? (imm.val, regB)
            else regB

    always do
      -- Trigger stage 5
      isSusp5 <== isSusp4.val
      warpId5 <== warpId4.val
      activeMask5 <== activeMask4.val
      instr5 <== instr4.val
      state5 <== state4.val
      go5 <== go4.val

    -- Stages 5 and 6: Execute and Writeback
    -- =====================================

    -- Functions to convert between 32-bit PC and instruction address
    let fromPC :: Bit 32 -> Bit t_logInstrs =
          \pc -> truncateCast (slice @31 @2 pc)
    let toPC :: Bit t_logInstrs -> Bit 32 =
          \addr -> zeroExtendCast addr # (0 :: Bit 2)

    -- Buffer the decode tables
    let tagMap5 = Map.map buffer tagMap4

    -- Insert warp id back into warp queue, except on warp command
    always do
      when (go5.val) do
        if inputs.simtWarpCmdWire.active
          then do
            -- We assume that warp has converged
            dynamicAssert (activeMask5.val .==. ones)
              "SIMT pipeline: warp command issued by diverged warp"
            -- Handle command
            if inputs.simtWarpCmdWire.val.warpCmd_isTerminate
              then do
                completedWarps <== completedWarps.val + 1
                -- Track kernel success
                let success = kernelSuccess.val .&.
                      inputs.simtWarpCmdWire.val.warpCmd_termCode
                -- Have all warps have terminated?
                if completedWarps.val .==. ones
                  then do
                    -- Issue kernel response to CPU
                    dynamicAssert (kernelRespQueue.notFull)
                      "SIMT pipeline: can't issue kernel response"
                    enq kernelRespQueue success
                    -- Re-enter initial state
                    pipelineActive <== false
                    kernelSuccess <== true
                  else do
                    -- Update kernel success
                    kernelSuccess <== success
              else do
                -- Enter barrier
                barrierBits!(warpId5.val) <== true
          else do
            -- Not a SIMT command, so reschedule
            dynamicAssert (warpQueue.notFull) "SIMT warp queue overflow"
            enq warpQueue (warpId5.val)

    -- Vector lane definition
    let makeLane makeExecStage threadActive suspMask regFileA
                 regFileB stateMem incCallDepth decCallDepth = do

          -- Per lane interfacing
          pcNextWire :: Wire (Bit t_logInstrs) <-
            makeWire (state5.val.simtPC + 1)
          retryWire  :: Wire (Bit 1) <- makeWire false
          suspWire   :: Wire (Bit 1) <- makeWire false
          resultWire :: Wire (Bit 32) <- makeWire dontCare

          -- Instantiate execute stage
          execStage <- makeExecStage
            State {
              instr = instr5.val
            , opA = regFileA.out.old
            , opB = regFileB.out.old
            , opBorImm = regFileB.out.getRegBOrImm.old
            , pc = ReadWrite (state5.val.simtPC.toPC) \writeVal -> do
                     pcNextWire <== writeVal.fromPC
            , result = WriteOnly \writeVal ->
                         when (instr5.val.dst .!=. 0) do
                           resultWire <== writeVal
            , suspend = do
                suspWire <== true
                return
                  InstrInfo {
                    instrId = warpId5.val.zeroExtendCast
                  , instrDest = instr5.val.dst
                  }
            , retry = retryWire <== true
            , opcode = packTagMap tagMap5
            }

          always do
            -- Execute stage
            when (go5.val .&. threadActive .&. isSusp5.val.inv) do
              -- Trigger execute stage
              execStage.execute

              -- Only update PC if not retrying
              when (retryWire.val.inv) do
                -- Compute new call depth increment
                let inc = incCallDepth.zeroExtendCast
                let dec = decCallDepth.zeroExtendCast
                dynamicAssert (state5.val.simtCallDepth .!=. 0)
                  "SIMT pipeliene: call depth overflow"
                store stateMem (warpId5.val)
                  SIMTThreadState {
                      simtPC = pcNextWire.val
                      -- Remember, lower is deeper
                    , simtCallDepth = (state5.val.simtCallDepth - inc) + dec
                    }

              -- Update suspension bits
              when (suspWire.val) do
                suspMask!(warpId5.val) <== true

            -- Writeback stage
            if resultWire.active.old
              then do
                let idx = (warpId5.val.old, instr5.val.dst.old)
                let value = resultWire.val.old
                store regFileA idx value
                store regFileB idx value
              else do
                -- Thread resumption
                when (execStage.resumeReqs.canPeek) do
                  let req = execStage.resumeReqs.peek
                  let idx = (req.resumeReqInfo.instrId.truncateCast,
                             req.resumeReqInfo.instrDest)
                  when (req.resumeReqInfo.instrDest .!=. 0) do
                    store regFileA idx (req.resumeReqData)
                    store regFileB idx (req.resumeReqData)
                  suspMask!(req.resumeReqInfo.instrId) <== false
                  execStage.resumeReqs.consume

    -- Create vector lanes
    sequence $ getZipList $
      makeLane <$> ZipList (c.executeStage)
               <*> ZipList (activeMask5.val.toBitList)
               <*> ZipList suspBits
               <*> ZipList regFilesA
               <*> ZipList regFilesB
               <*> ZipList stateMems
               <*> ZipList (inputs.simtIncCallDepth)
               <*> ZipList (inputs.simtDecCallDepth)

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
          -- Clear barrier bit
          barrierBits!warpId <== false
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

    -- Pipeline outputs
    return
      SIMTPipelineOuts {
        simtMgmtResps = kernelRespQueue.toStream
      , simtCurrentWarpId = warpId5.val.zeroExtendCast
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
