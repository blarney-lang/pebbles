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
import qualified Data.Map as Map

-- Pebbles imports
import Pebbles.Pipeline.Interface

-- | Scalar pipeline configuration
data ScalarPipelineConfig =
  ScalarPipelineConfig {
    -- | Instruction memory initilaisation file
    instrMemInitFile :: Maybe String
    -- | Instruciton memory size
  , instrMemLogNumInstrs :: Int
    -- | Decode table
  , decodeStage :: [(String, String)]
    -- | Action for execute stage
  , executeStage :: DecodeInfo -> State -> Action ()
    -- | Resumption for multi-cycle instructions
  , resumeStage :: Stream ResumeReq
  }

-- | Scalar pipeline management
data ScalarPipeline =
  ScalarPipeline {
    -- Write to instruction memory
    writeInstr :: Bit 32 -> Bit 32 -> Action ()
  }

-- | Scalar pipeline
makeScalarPipeline :: ScalarPipelineConfig -> Module ScalarPipeline
makeScalarPipeline c = 
  -- Determine instruction mem address width at type level
  liftNat (c.instrMemLogNumInstrs) \(_ :: Proxy t_instrAddrWidth) -> do

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

    -- Instruction operand registers
    regA :: Reg (Bit 32) <- makeReg dontCare
    regB :: Reg (Bit 32) <- makeReg dontCare
    regBorImm :: Reg (Bit 32) <- makeReg dontCare

    -- Wire used to override the update to the PC,
    -- in case of a branch instruction,
    -- which also triggers a pipeline flush
    pcNext :: Wire (Bit 32) <- makeWire dontCare

    -- Pipeline stall
    stallWire :: Wire (Bit 1) <- makeWire 0

    -- Invariant: pipeline stall and flush should never happen at same time
    always do
      when (stallWire.val .&. pcNext.active) do
        display "Pipeline assertion failed: simultaneous stall and flush"

    -- Result of the execute stage
    resultWire :: Wire (Bit 32) <- makeWire dontCare
    resumeResultWire :: Wire (Bit 32) <- makeWire dontCare
    finalResultWire :: Wire (Bit 32) <- makeWire dontCare

    -- Is there a multi-cycle instruction in progress?
    suspendInProgress :: Reg (Bit 1) <- makeReg false

    -- Did instruction in execute stage request a retry?
    retryWire :: Wire (Bit 1) <- makeWire false

    -- Program counters for each pipeline stage
    pc1 :: Reg (Bit 32) <- makeReg 0xfffffffc
    pc2 :: Reg (Bit 32) <- makeReg dontCare
    pc3 :: Reg (Bit 32) <- makeReg dontCare

    -- Instruction registers for each pipeline stage
    instr2 :: Reg Instr <- makeReg 0
    instr3 :: Reg Instr <- makeReg 0
    instr4 :: Reg Instr <- makeReg 0

    -- Triggers for each pipeline stage
    go1 :: Reg (Bit 1) <- makeDReg false
    go2 :: Reg (Bit 1) <- makeDReg false
    go3 :: Reg (Bit 1) <- makeDReg false

    always do
      -- Stage 0: Instruction Fetch
      -- ==========================

      -- PC to fetch
      let pcFetch = pcNext.active ? (pcNext.val, pc1.val + 4)

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

      -- Stage 1: Operand Fetch
      -- ======================

      when (go1.val) do
        -- Trigger stage 2 when not flushing or stalling
        when (pcNext.active.inv .&. stallWire.val.inv) do
          go2 <== true
          instr2 <== instrMem.out
          pc2 <== pc1.val

      -- Fetch operands
      load regFileA (stallWire.val ? (instr2.val.srcA, instrMem.out.srcA))
      load regFileB (stallWire.val ? (instr2.val.srcB, instrMem.out.srcB))

      -- Stage 2: Latch Operands
      -- =======================

      -- Decode instruction
      let (tagMap, fieldMap) = matchMap False (c.decodeStage) (instr2.val)

      -- Register forwarding from stage 3
      let forward3 rS other =
           (resultWire.active .&. (instr3.val.dst .==. instr2.val.rS)) ?
           (resultWire.val, other)

      -- Register forwarding from stage 4
      let forward4 rS other =
           (finalResultWire.active .&.
             (instr4.val.dst .==. instr2.val.rS)) ?
               (finalResultWire.val, other)

      -- Register forwarding
      let a = forward3 srcA (forward4 srcA (regFileA.out))
      let b = forward3 srcB (forward4 srcB (regFileB.out))

      -- Use "imm" field if valid, otherwise use register b
      let bOrImm = if Map.member "imm" fieldMap
                     then let imm = getField fieldMap "imm"
                          in imm.valid ? (imm.val, b)
                     else b

      when (go2.val) do
        -- Trigger stage 3 when not flushing or stalling
        when (pcNext.active.inv .&. stallWire.val.inv) do
          -- Latch operands
          regA <== a
          regB <== b
          regBorImm <== bOrImm

          -- Trigger next stage
          go3 <== true
          instr3 <== instr2.val
          pc3 <== pc2.val

      -- Stage 3: Execute
      -- ================

      -- Buffer the decode tables
      let bufferEn = delayEn dontCare
      let bufferField en opt =
            Option (bufferEn en (opt.valid)) (map (bufferEn en) (opt.val))
      let tagMap3 = Map.map (bufferEn (stallWire.val.inv)) tagMap
      let fieldMap3 = Map.map (bufferField (stallWire.val.inv)) fieldMap

      -- Information from decode stage
      let decodeInfo =
            DecodeInfo {
              opcode = tagMap3
            , fields = fieldMap3
            }

      -- State for execute stage
      let state = State {
              instr = instr3.val
            , opA = regA.val
            , opB = regB.val
            , opBorImm = regBorImm.val
            , pc = ReadWrite (pc3.val) (pcNext <==)
            , result = WriteOnly \x ->
                         when (instr3.val.dst .!=. 0) do
                           resultWire <== x
            , suspend = do
                -- In future, we could allow independent instructions
                -- to bypass multi-cycle instructions
                suspendInProgress <== true
                stallWire <== true
                return dontCare
            , retry = do
                go3 <== true
                retryWire <== true
                stallWire <== true
            }

      -- Execute stage
      when (go3.val) do
        executeStage c decodeInfo state
        when (retryWire.val.inv) do
          instr4 <== instr3.val

      -- Stage 4: Writeback
      -- ==================

      -- Resume stage for multi-cycle instructions
      let resumeReqs = c.resumeStage
      if resumeReqs.canPeek
        then do
          resumeReqs.consume
          when (instr4.val.dst .!=. 0) do
            resumeResultWire <== resumeReqs.peek.resumeReqData
          suspendInProgress <== false
        else do
          -- Stall while waiting for response
          when (suspendInProgress.val) do
            stallWire <== true

      -- Determine final result
      let rd = instr4.val.dst
      when (resumeResultWire.active) do
        finalResultWire <== resumeResultWire.val
      when (resumeResultWire.active.inv .&. delay 0 (resultWire.active)) do
        finalResultWire <== resultWire.val.old

      -- Writeback
      when (finalResultWire.active) do
        store regFileA rd (finalResultWire.val)
        store regFileB rd (finalResultWire.val)

    -- Pipeline management interface
    return
      ScalarPipeline {
        writeInstr = \addr instr -> do
          store instrMem (truncateCast (slice @31 @2 addr)) instr
      }
