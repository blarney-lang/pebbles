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

-- | Scalar pipeline configuration
data ScalarPipelineConfig tag =
  ScalarPipelineConfig {
    instrMemInitFile :: Maybe String
    -- ^ Instruction memory initilaisation file
  , instrMemLogNumInstrs :: Int
    -- ^ Instruction memory size
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
  , checkPCCFunc :: Maybe (InternalCap -> [(Bit 1, TrapCode)])
    -- ^ When CHERI is enabled, function to check PCC
  }

-- | Scalar pipeline management
data ScalarPipeline =
  ScalarPipeline {
    writeInstr :: Bit 32 -> Bit 32 -> Action ()
    -- ^ Write to instruction memory
  }

-- | Scalar pipeline
makeScalarPipeline :: Tag tag =>
     ScalarPipelineConfig tag
     -- ^ Inputs: pipeline configuration
  -> Module ScalarPipeline
     -- ^ Outputs: pipeline management interface
makeScalarPipeline c = 
  -- Determine instruction mem address width at type level
  liftNat (c.instrMemLogNumInstrs) \(_ :: Proxy t_instrAddrWidth) -> do

    -- Is CHERI enabled?
    let enableCHERI = c.checkPCCFunc.isJust

    -- Compute field selector functions from decode table
    let selMap = matchSel (c.decodeStage)

    -- Functions for extracting register ids from an instruction
    let srcA :: Instr -> RegId = getFieldSel selMap "rs1"
    let srcB :: Instr -> RegId = getFieldSel selMap "rs2"
    let dst  :: Instr -> RegId = getFieldSel selMap "rd"

    -- Instruction memory
    instrMem :: RAM (Bit t_instrAddrWidth) Instr <-
      makeDualRAMCore (c.instrMemInitFile)

    -- Two block RAMs allows two operands to be read,
    -- and one result to be written, on every cycle
    regFileA :: RAM RegId (Bit 32) <- makeDualRAMForward 0
    regFileB :: RAM RegId (Bit 32) <- makeDualRAMForward 0

    -- Capability register file (meta data only)
    capFileA :: RAM RegId InternalCapMetaData <-
      if not enableCHERI then return nullRAM else
        case c.capRegInitFile of
          Nothing -> makeDualRAMForward 0
          Just file -> makeDualRAMForwardInit 0 file
    capFileB :: RAM RegId InternalCapMetaData <-
      if not enableCHERI then return nullRAM else
        case c.capRegInitFile of
          Nothing -> makeDualRAMForward 0
          Just file -> makeDualRAMForwardInit 0 file

    -- Instruction operand registers
    regA :: Reg (Bit 32) <- makeReg dontCare
    regB :: Reg (Bit 32) <- makeReg dontCare
    regBorImm :: Reg (Bit 32) <- makeReg dontCare

    -- Capbility operand registers (meta-data only)
    capA :: Reg InternalCapMetaData <- makeReg dontCare
    capB :: Reg InternalCapMetaData <- makeReg dontCare

    -- Wire used to override the update to the PC,
    -- in case of a branch instruction,
    -- which also triggers a pipeline flush
    pcNext :: Wire (Bit 32) <- makeWire dontCare

    -- Wire used to update the PCC meta data
    pccNext :: Wire InternalCap <- makeWire dontCare

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
    resultCapWire :: Wire InternalCapMetaData <- makeWire dontCare
    finalResultCapWire :: Wire InternalCapMetaData <- makeWire dontCare

    -- Is there a multi-cycle instruction in progress?
    suspendInProgress :: Reg (Bit 1) <- makeReg false

    -- Did instruction in execute stage request a retry?
    retryWire :: Wire (Bit 1) <- makeWire false

    -- Program counters for each pipeline stage
    pc1 :: Reg (Bit 32) <- makeReg (fromInteger (c.initialPC - 4))
    pc2 :: Reg (Bit 32) <- makeReg dontCare
    pc3 :: Reg (Bit 32) <- makeReg dontCare

    -- Program counter capabilities for each pipeline stage
    pcc1 :: Reg InternalCap <- makeReg almightyCapVal
    pcc2 :: Reg (Exact InternalCap) <- makeReg dontCare
    pcc3 :: Reg InternalCap <- makeReg dontCare

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
      -- PC for fetch
      let pcFetch = pcNext.active ? (pcNext.val, pc1.val + 4)

      -- PC capability for fetch
      let pccFetch = pccNext.active ? (pccNext.val, pcc1.val)

      -- Index the instruction memory
      let instrAddr = truncateCast (slice @31 @2 pcFetch)
      load instrMem instrAddr

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
        when (pcNext.active.inv .&. stallWire.val.inv) do
          go2 <== true
          instr2 <== instrMem.out
          pc2 <== pc1.val

          if enableCHERI
            then pcc2 <== setAddr (pcc1.val) (pc1.val)
            else return ()

      -- Register file indicies
      let idxA = stallWire.val ? (instr2.val.srcA, instrMem.out.srcA)
      let idxB = stallWire.val ? (instr2.val.srcB, instrMem.out.srcB)

      -- Fetch operands
      load regFileA idxA
      load regFileB idxB

      if enableCHERI
        then do
          load capFileA idxA
          load capFileB idxB
        else return ()

    -- Stage 2: Latch Operands
    -- =======================

    -- Decode instruction
    let (tagMap, fieldMap) = matchMap False (c.decodeStage) (instr2.val)

    -- Register forwarding from stage 3
    let forward3 :: Bits t => Wire t -> (Instr -> RegId) -> t -> t
        forward3 resWire rS other =
          (resWire.active .&. (instr3.val.dst .==. instr2.val.rS)) ?
          (resWire.val, other)

    -- Register forwarding from stage 4
    let forward4 :: Bits t => Wire t -> (Instr -> RegId) -> t -> t
        forward4 resWire rS other =
          (resWire.active .&.
            (instr4.val.dst .==. instr2.val.rS)) ?
              (resWire.val, other)

    -- Register forwarding
    let a = forward3 resultWire srcA $
              forward4 finalResultWire srcA (regFileA.out)
    let b = forward3 resultWire srcB $
              forward4 finalResultWire srcB (regFileB.out)

    -- Use "imm" field if valid, otherwise use register b
    let bOrImm = if Map.member "imm" fieldMap
                   then let imm = getField fieldMap "imm"
                        in imm.valid ? (imm.val, b)
                   else b

    -- Capability register forwarding
    let a_cap = forward3 resultCapWire srcA $
                  forward4 finalResultCapWire srcA (capFileA.out)
    let b_cap = forward3 resultCapWire srcB $
                  forward4 finalResultCapWire srcB (capFileB.out)

    always do
      when (go2.val) do
        -- Trigger stage 3 when not flushing or stalling
        when (pcNext.active.inv .&. stallWire.val.inv) do
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
              let pcc = pcc2.val
              let table = checkPCC (pcc.value)
              let exception = orList [cond | (cond, _) <- table]
              let ok = pcc.exact .&&. inv exception
              go3 <== ok .&&. trapWire.val.inv
              pcc3 <== pcc.value
              pcc3_exc <== inv ok
              when (inv ok) do
                let trapCode = priorityIf table dontCare
                display "Scalar pipeline: PCC exception:"
                  " code=" trapCode
                  " pc=" (formatHex 8 (pc2.val))

    -- Stage 3: Execute
    -- ================

    -- Buffer the decode tables
    let bufferEn = delayEn dontCare
    let tagMap3 = Map.map (bufferEn (stallWire.val.inv)) tagMap

    -- Is destination register non-zero?
    let destNonZero = instr3.val.dst .!=. 0

    -- Isntantiate execute stage
    execStage <- executeStage c
      State {
        instr = instr3.val
      , opA = regA.val
      , opB = regB.val
      , opBorImm = regBorImm.val
      , capA = unsplitCap (capA.val, regA.val)
      , capB = unsplitCap (capB.val, regB.val)
      , opAIndex = instr3.val.srcA
      , opBIndex = instr3.val.srcB
      , resultIndex = instr3.val.dst
      , pc = ReadWrite (pc3.val) (pcNext <==)
      , pcc = if enableCHERI
                then ReadWrite (pcc3.val) (pccNext <==)
                else error "Scalar Pipeline: PCC used when CHERI disabled"
      , result = WriteOnly \x ->
                   when destNonZero do
                     resultWire <== x
                     if enableCHERI
                       then resultCapWire <== nullCapMetaVal
                       else return ()
      , resultCap = WriteOnly \cap ->
                      when destNonZero do
                        let (meta, addr) = splitCap cap
                        resultWire <== addr
                        resultCapWire <== meta
      , suspend = do
          -- In future, we could allow independent instructions
          -- to bypass multi-cycle instructions
          suspendInProgress <== true
          stallWire <== true
          return
            InstrInfo {
              -- Some of these fields are not used because the
              -- pipeline currently blocks for suspended instrs
              instrId = dontCare
            , instrDest = dontCare
            , instrTagMask = false
            }

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
      }

    always do
      -- Invoke execute stage
      when (go3.val) do
        execStage.execute
        when (retryWire.val.inv) do
          instr4 <== instr3.val

      -- Common trap-handling code
      when (trapReg .||. pcc3_exc.val) do
        c.trapCSRs.csr_mepc <== if trapReg then pc3.val.old else pc3.val
        pcNext <== slice @31 @2 (c.trapCSRs.csr_mtvec.val) # 0b00

    -- Stage 4: Writeback
    -- ==================

    always do
      let resumeReq = execStage.resumeReqs.peek
      -- Resume stage for multi-cycle instructions
      if execStage.resumeReqs.canPeek
        then do
          execStage.resumeReqs.consume
          when (instr4.val.dst .!=. 0) do
            resumeResultWire <== true
          suspendInProgress <== false
        else do
          -- Stall while waiting for response
          when (suspendInProgress.val) do
            stallWire <== true

      -- Determine final result
      let rd = instr4.val.dst
      if resumeResultWire.val
        then do
          finalResultWire <== resumeReq.resumeReqData
          if enableCHERI
            then do
              when (resumeReq.resumeReqCap.isSome) do
                finalResultCapWire <== resumeReq.resumeReqCap.val
            else return ()
        else do
          when (inv trapReg) do
            when (delay false (resultWire.active)) do
              finalResultWire <== resultWire.val.old
            if enableCHERI
              then do
                when (delay false (resultCapWire.active)) do
                  finalResultCapWire <== resultCapWire.val.old
              else return ()

      -- Writeback
      when (finalResultWire.active) do
        store regFileA rd (finalResultWire.val)
        store regFileB rd (finalResultWire.val)

      if enableCHERI
        then do
          when (finalResultCapWire.active) do
            store capFileA rd (finalResultCapWire.val)
            store capFileB rd (finalResultCapWire.val)
        else return ()

    -- Pipeline outputs
    return
      ScalarPipeline {
        writeInstr = \addr instr -> do
          store instrMem (truncateCast (slice @31 @2 addr)) instr
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
