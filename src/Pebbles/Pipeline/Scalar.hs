module Pebbles.Pipeline.Scalar where

-- 32-bit, 5-stage, scalar, in-order pipeline
-- with register-forwarding and static branch prediction.
--
-- The 5 pipeline stages are:
--
--  0. Intruction Fetch
--  1. Operand Fetch
--  2. Operand Latch
--  3. Execute (& Instruction Suspension)
--  4. Writeback (& Instruction Resumption)
--
-- The Operand Latch stage is a bit unusual, but removes the register
-- file read delay from combinatorial path of the Execute stage.

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.Stream
import Blarney.BitScan

-- General imports
import Data.Proxy
import Data.Maybe
import qualified Data.Map as Map

-- Pebbles imports
import Pebbles.CSRs.Trap
import Pebbles.CSRs.TrapCodes
import Pebbles.Pipeline.Interface

-- CHERI imports
import CHERI.CapLib

-- | Info about multi-cycle instructions issued by pipeline
type ScalarPipelineInstrInfo = ()

-- | Scalar pipeline configuration
data ScalarPipelineConfig tag =
  ScalarPipelineConfig {
    instrMemInitFile :: Maybe String
    -- ^ Instruction memory initilaisation file
  , instrMemLogNumInstrs :: Int
    -- ^ Instruction memory size
  , enableRegForwarding :: Bool
    -- ^ Enable register forwarding for higher IPC but lower Fmax
  , initialPC :: Integer
    -- ^ Initial program counter
  , capRegInitFile :: Maybe String
    -- ^ File containing initial capability reg file meta-data
  , decodeStage :: [(String, tag)]
    -- ^ Decode table
  , executeStage :: State -> Module ExecuteStage
    -- ^ Action for execute stage
  , trapCSRs :: TrapCSRs
    -- ^ Trap-related CSRs
  , checkPCCFunc :: Maybe (Cap -> [(Bit 1, TrapCode)])
    -- ^ When CHERI is enabled, function to check PCC
  }

-- | Scalar pipeline inputs
data ScalarPipelineIns =
  ScalarPipelineIns {
    resumeReqs :: Stream (ScalarPipelineInstrInfo, ResumeReq)
    -- ^ Resume requests for multi-cycle instructions
  }

-- | Scalar pipeline outputs
data ScalarPipelineOuts =
  ScalarPipeline {
    writeInstr :: Bit 32 -> Bit 32 -> Action ()
    -- ^ Write to instruction memory
  , instrInfo :: ScalarPipelineInstrInfo
    -- ^ Info for instruction currently in execute stage
  }

-- | Scalar pipeline
makeScalarPipeline :: Tag tag =>
     ScalarPipelineConfig tag
     -- ^ Pipeline configuration
  -> ScalarPipelineIns
     -- ^ Pipeline inputs
  -> Module ScalarPipelineOuts
     -- ^ Pipeline outputs
makeScalarPipeline c pipeIns = 
  -- Determine instruction mem address width at type level
  liftNat (c.instrMemLogNumInstrs) \(_ :: Proxy t_instrAddrWidth) -> do

    -- Is CHERI enabled?
    let enableCHERI = isJust c.checkPCCFunc

    -- Compute field selector functions from decode table
    let selMap = matchSel (c.decodeStage)

    -- Functions for extracting register ids from an instruction
    let srcA :: Instr -> RegId = getBitFieldSel selMap "rs1"
    let srcB :: Instr -> RegId = getBitFieldSel selMap "rs2"
    let dst  :: Instr -> RegId = getBitFieldSel selMap "rd"

    -- Instruction memory
    instrMem :: RAM (Bit t_instrAddrWidth) Instr <-
      makeDualRAMCore (c.instrMemInitFile)

    -- Two block RAMs allows two operands to be read,
    -- and one result to be written, on every cycle
    regFileA :: RAM RegId (Bit 32) <- makeDualRAMForward
    regFileB :: RAM RegId (Bit 32) <- makeDualRAMForward

    -- Capability register file (meta data only)
    capFileA :: RAM RegId CapPipeMeta <-
      if not enableCHERI then return nullRAM else
        case c.capRegInitFile of
          Nothing -> makeDualRAMForward
          Just file -> makeDualRAMForwardInit file
    capFileB :: RAM RegId CapPipeMeta <-
      if not enableCHERI then return nullRAM else
        case c.capRegInitFile of
          Nothing -> makeDualRAMForward
          Just file -> makeDualRAMForwardInit file

    -- Instruction operand registers
    regA :: Reg (Bit 32) <- makeReg dontCare
    regB :: Reg (Bit 32) <- makeReg dontCare
    regBorImm :: Reg (Bit 32) <- makeReg dontCare

    -- Capbility operand registers (meta-data only)
    capA :: Reg CapPipeMeta <- makeReg dontCare
    capB :: Reg CapPipeMeta <- makeReg dontCare

    -- Wire used to override the update to the PC,
    -- in case of a branch instruction,
    -- which also triggers a pipeline flush
    pcNext :: Wire (Bit 32) <- makeWire dontCare

    -- Wire used to update the PCC
    pccNext :: Wire CapPipe <- makeWire dontCare

    -- Pipeline stall
    stallWire :: Wire (Bit 1) <- makeWire 0

    -- Invariant: pipeline stall and flush should never happen at same time
    always do
      when (stallWire.val .&. pcNext.active) do
        display "Pipeline assertion failed: simultaneous stall and flush"

    -- Result of the execute stage
    resultWire :: Wire (Bit 32) <- makeWire dontCare
    resumeResultWire :: Wire (Bit 1) <- makeWire false
    finalResultWire :: Wire (Bit 32) <- makeWire dontCare

    -- Result of the execute stage (capability meta-data)
    resultCapWire :: Wire CapPipeMeta <- makeWire dontCare
    finalResultCapWire :: Wire CapPipeMeta <- makeWire dontCare

    -- Is there a multi-cycle instruction in progress?
    suspendInProgress :: Reg (Bit 1) <- makeReg false

    -- Did instruction in execute stage request a retry?
    retryWire :: Wire (Bit 1) <- makeWire false

    -- Program counters for each pipeline stage
    pc1 :: Reg (Bit 32) <- makeReg (fromInteger (c.initialPC - 4))
    pc2 :: Reg (Bit 32) <- makeReg dontCare
    pc3 :: Reg (Bit 32) <- makeReg dontCare

    -- Program counter capability meta-data for each pipeline stage
    pcc1 :: Reg CapPipe <- makeReg almightyCapPipeVal
    pcc2 :: Reg Cap <- makeReg dontCare
    pcc3 :: Reg Cap <- makeReg dontCare

    -- Program counter capability check
    pcc3_exc :: Reg (Bit 1) <- makeDReg false

    -- Instruction registers for each pipeline stage
    instr2 :: Reg Instr <- makeReg 0
    instr3 :: Reg Instr <- makeReg 0
    instr4 :: Reg Instr <- makeReg 0

    -- Triggers for each pipeline stage
    go1 :: Reg (Bit 1) <- makeDReg false
    go2 :: Reg (Bit 1) <- makeDReg false
    go3 :: Reg (Bit 1) <- makeDReg false

    -- Wire pulsed to signal an exception / interrupt
    trapWire :: Wire (Bit 1) <- makeWire false

    -- See [Note: Delayed Trap]
    let trapReg = delay false (trapWire.val)

    -- Stage 0: Instruction Fetch
    -- ==========================

    always do
      let pcFetch = pcNext.active ? (pcNext.val, pc1.val + 4)

      -- PC capability meta-data for fetch
      let pccFetch = pccNext.active ? (pccNext.val, pcc1.val)

      -- Index the instruction memory
      let instrAddr = truncateCast (slice @31 @2 pcFetch)
      instrMem.load instrAddr

      -- Handle stall
      if stallWire.val
        then do
          instrMem.preserveOut
          go1 <== go1.val
          go2 <== go2.val
        else do
          pc1 <== pcFetch
          go1 <== true

          if enableCHERI
            then pcc1 <== pccFetch
            else return ()

    -- Stage 1: Operand Fetch
    -- ======================

    always do
      when (go1.val) do
        -- Trigger stage 2 when not flushing or stalling
        when (inv pcNext.active .&. inv stallWire.val) do
          go2 <== true
          instr2 <== instrMem.out
          pc2 <== pc1.val

          if enableCHERI
            then do
              -- Ensure the PCC is representable
              let cap = setAddr (pcc1.val) (pc1.val)
              pcc2 <== decodeCapPipe (cap.value)
            else return ()

      -- Register file indicies
      let idxA = stallWire.val ? (srcA instr2.val, srcA instrMem.out)
      let idxB = stallWire.val ? (srcB instr2.val, srcB instrMem.out)

      -- Fetch operands
      regFileA.load idxA
      regFileB.load idxB

      if enableCHERI
        then do
          capFileA.load idxA
          capFileB.load idxB
        else return ()

    -- Stage 2: Latch Operands
    -- =======================

    -- Decode instruction
    let (tagMap, fieldMap) = matchMap False (c.decodeStage) (instr2.val)

    -- Data hazard from stage 3
    let hazard3 :: Bits t => Wire t -> (Instr -> RegId) -> Bit 1
        hazard3 resWire rS = resWire.active .&&.
          dst instr3.val .==. rS instr2.val

    -- Data hazard from stage 4
    let hazard4 :: Bits t => Wire t -> (Instr -> RegId) -> Bit 1
        hazard4 resWire rS = resWire.active .&&.
          dst instr4.val .==. rS instr2.val

    -- Register forwarding from stage 3
    let forward3 :: Bits t => Wire t -> (Instr -> RegId) -> t -> t
        forward3 resWire rS other =
          hazard3 resWire rS ? (resWire.val, other)

    -- Register forwarding from stage 4
    let forward4 :: Bits t => Wire t -> (Instr -> RegId) -> t -> t
        forward4 resWire rS other =
          hazard4 resWire rS ? (resWire.val, other)

    -- Register forwarding
    let a = if c.enableRegForwarding
              then forward3 resultWire srcA $
                     forward4 finalResultWire srcA (regFileA.out)
              else regFileA.out
    let b = if c.enableRegForwarding
              then forward3 resultWire srcB $
                     forward4 finalResultWire srcB (regFileB.out)
              else regFileB.out

    -- Use "imm" field if valid, otherwise use register b
    let bOrImm = if Map.member "imm" fieldMap
                   then let imm = getBitField fieldMap "imm"
                        in imm.valid ? (imm.val, b)
                   else b

    -- Capability register forwarding
    let a_cap = if c.enableRegForwarding
                  then forward3 resultCapWire srcA $
                         forward4 finalResultCapWire srcA (capFileA.out)
                  else capFileA.out
    let b_cap = if c.enableRegForwarding
                  then forward3 resultCapWire srcB $
                         forward4 finalResultCapWire srcB (capFileB.out)
                  else capFileB.out

    -- Is there a data hazard?
    let isDataHazard =
          if c.enableRegForwarding
            then false
            else orList
                   [ haz
                   | src <- [srcA, srcB]
                   , haz <- [hazard3 resultWire src,
                             hazard4 finalResultWire src,
                             hazard3 resultCapWire src,
                             hazard4 finalResultCapWire src]
                   ]

    always do
      when (go2.val .&&. inv pcNext.active) do
        -- Stall on data hazard
        when isDataHazard do stallWire <== true
        -- Trigger stage 3 when not stalling
        when (inv stallWire.val) do
          -- Latch operands
          regA <== a
          regB <== b
          regBorImm <== bOrImm

          if enableCHERI
            then do
              capA <== a_cap
              capB <== b_cap
            else return ()

          -- Prepare next stage
          instr3 <== instr2.val
          pc3 <== pc2.val

          -- Trigger next stage
          case c.checkPCCFunc of
            Nothing -> do
              go3 <== true
            Just checkPCC -> do
              let table = checkPCC (pcc2.val)
              let exception = orList [cond | (cond, _) <- table]
              go3 <== inv exception .&&. inv trapWire.val
              pcc3 <== pcc2.val
              pcc3_exc <== exception
              when exception do
                let trapCode = priorityIf table dontCare
                display "Scalar pipeline: PCC exception:"
                  " code=" trapCode
                  " pc=" (formatHex 0 $ pc2.val)
                  " capPipe=" (formatHex 0 $ pcc2.val.capPipe)
                finish

    -- Stage 3: Execute
    -- ================

    -- Buffer the decode tables
    let bufferEn = delayEn dontCare
    let tagMap3 = Map.map (bufferEn (inv stallWire.val)) tagMap

    -- Is destination register non-zero?
    let destNonZero = dst instr3.val .!=. 0

    -- Isntantiate execute stage
    execStage <- executeStage c
      State {
        instr = instr3.val
      , opA = regA.val
      , opB = regB.val
      , opBorImm = regBorImm.val
      , opAIndex = srcA instr3.val
      , opBIndex = srcB instr3.val
      , resultIndex = dst instr3.val
      , pc = ReadWrite (pc3.val) \pcNew -> do
               pcNext <== pcNew
      , result = WriteOnly \x ->
                   when destNonZero do
                     resultWire <== x
                     if enableCHERI
                       then resultCapWire <== nullCapPipeMetaVal
                       else return ()
      , suspend = do
          -- In future, we could allow independent instructions
          -- to bypass multi-cycle instructions
          suspendInProgress <== true
          stallWire <== true

      , retry = do
          go3 <== true
          retryWire <== true
          stallWire <== true
      , opcode = packTagMap tagMap3
      , trap = \code -> do
          display "Trap: code=" code " pc=0x" (formatHex 8 (pc3.val))
          c.trapCSRs.csr_mcause <==
            code.trapCodeIsInterrupt # code.trapCodeCause
          trapWire <== true
      , capA = decodeCapPipe $ unsplitCapPipe (capA.val, regA.val)
      , capB = decodeCapPipe $ unsplitCapPipe (capB.val, regB.val)
      , pcc = pcc3.val
      , pccNew = WriteOnly \pccNew -> do
                   pcNext <== getAddr pccNew
                   pccNext <== pccNew
      , resultCap = WriteOnly \cap ->
                      when destNonZero do
                        let (meta, addr) = splitCapPipe cap
                        resultWire <== addr
                        resultCapWire <== meta
      }

    always do
      -- Invoke execute stage
      when (go3.val) do
        execStage.execute
        when (inv retryWire.val) do
          instr4 <== instr3.val

      -- Common trap-handling code
      when (trapReg .||. pcc3_exc.val) do
        c.trapCSRs.csr_mepc <== if trapReg then old pc3.val else pc3.val
        pcNext <== slice @31 @2 (c.trapCSRs.csr_mtvec.val) # 0b00

    -- Stage 4: Writeback
    -- ==================

    always do
      let resumeReq = pipeIns.resumeReqs.peek.snd
      -- Resume stage for multi-cycle instructions
      if pipeIns.resumeReqs.canPeek
        then do
          pipeIns.resumeReqs.consume
          when (dst instr4.val .!=. 0) do
            resumeResultWire <== true
          suspendInProgress <== false
        else do
          -- Stall while waiting for response
          when (suspendInProgress.val) do
            stallWire <== true

      -- Determine final result
      let rd = dst instr4.val
      if resumeResultWire.val
        then do
          finalResultWire <== resumeReq.resumeReqData
          if enableCHERI
            then do
              when (isSome resumeReq.resumeReqCap) do
                let (meta, addr) = splitCapPipe $ fromMem $ unpack
                     (resumeReq.resumeReqCap.val # resumeReq.resumeReqData)
                finalResultCapWire <== meta
            else return ()
        else do
          when (inv trapReg) do
            when (delay false (resultWire.active)) do
              finalResultWire <== old resultWire.val
            if enableCHERI
              then do
                when (delay false (resultCapWire.active)) do
                  finalResultCapWire <== old resultCapWire.val
              else return ()

      -- Writeback
      when (finalResultWire.active) do
        regFileA.store rd finalResultWire.val
        regFileB.store rd finalResultWire.val

      if enableCHERI
        then do
          when (finalResultCapWire.active) do
            capFileA.store rd finalResultCapWire.val
            capFileB.store rd finalResultCapWire.val
        else return ()

    -- Pipeline outputs
    return
      ScalarPipeline {
        writeInstr = \addr instr -> do
          instrMem.store (truncateCast (slice @31 @2 addr)) instr
      , instrInfo = ()
      }

-- Note [Delayed Trap]
-- ===================

-- Traps can be simply implemented by modifying pcNext.  However, ISA
-- extensions like CHERI can introduce a lot of logic on the trap
-- condition, so we delay the modification of pcNext until the cycle
-- after the trap has been raised, and prohibit the execute stage from
-- firing on that cycle.  We also prohibit the writeback stage from
-- firing on that cycle, allowing the ISA implementation to trap *and*
-- write a result at the same time, with the effect that result is
-- ignored. Separating the trap condition from the result writeback
-- reduces the amount of logic on the register forwarding path
-- (believed to be a critical path).
