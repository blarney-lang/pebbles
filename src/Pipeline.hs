module Pipeline where

-- 32-bit, 5-stage, scalar, in-order pipeline
-- with register-forwarding and static branch prediction

-- General imports
import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.Stream
import Blarney.BitScan
import qualified Data.Map as Map

-- Local imports
import DataMem

-- Instructions
type Instr = Bit 32

-- Register identifiers
type RegId = Bit 5

-- Instruction memory size
type InstrAddr = Bit 14

-- Pipeline configuration
data Config =
  Config {
    -- Decode table
    decodeStage :: [(String, String)]
    -- Action for execute stage
  , executeStage :: State -> Action ()
    -- Action for memory-response stage
  , memResponseStage :: State -> Bit 32 -> Action ()
    -- Resumption for multi-cycle instructions
  , resumeStage :: Stream ResumeReq
  }

-- Identifier for instruction suspension/resumption
-- (Unused for this scalar pipeline)
type InstrId = Bit 0

-- Resume request to pipeline for multi-cycle instructions
data ResumeReq =
  ResumeReq {
    -- Unique identifier that was given by the suspend call
    resumeReqId :: InstrId
    -- Data representing the result of the suspended operation
  , resumeReqData :: Bit 32
  } deriving (Generic, Bits)

-- Pipeline state, visisble to the instruction set
data State =
  State {
    -- Current instruction
    instr :: Bit 32
    -- Source operands
  , opA :: Bit 32
  , opB :: Bit 32
  , opBorImm :: Bit 32
    -- Program counter interface
  , pc :: ReadWrite (Bit 32)
    -- Write the instruction result
  , result :: WriteOnly (Bit 32)
    -- Memory access methods
  , memLoad :: Bit 32 -> Action ()
  , memStore :: Bit 32 -> Bit 4 -> Bit 32 -> Action ()
    -- Call this to implement a multi-cycle instruction
    -- Results are returned via resume stage
  , suspend :: Action InstrId
    -- Call this if instruction cannot currently be executed
    -- (Perhaps resources are not currently available)
  , retry :: Action ()
    -- Result of instruction decode
  , opcode :: TagMap String
  , fields :: FieldMap
  }

-- TODO: implement load/store via suspend+retry+resume?

-- Pipeline
makePipeline :: Bool -> Config -> Stream MemResp -> Module (Stream MemReq)
makePipeline sim c memResps = do
  -- Compute field selector functions from decode table
  let selMap = matchSel (c.decodeStage)

  -- Functions for extracting register ids from an instruction
  let srcA :: Instr -> RegId = getFieldSel selMap "rs1"
  let srcB :: Instr -> RegId = getFieldSel selMap "rs2"
  let dst  :: Instr -> RegId = getFieldSel selMap "rd"

  -- Instruction memory
  let ext = if sim then ".hex" else ".mif"
  instrMem :: RAM InstrAddr Instr <- makeRAMInit ("prog" ++ ext)

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
  memRespResultWire :: Wire (Bit 32) <- makeWire dontCare
  finalResultWire :: Wire (Bit 32) <- makeWire dontCare

  -- Memory request queue
  memReqQueue :: Queue MemReq <- makeBypassQueue

  -- Is there a request in flight?
  memReqInFlight :: Reg (Bit 1) <- makeReg false

  -- Is there a multi-cycle instruction in progress?
  suspendInProgress :: Reg (Bit 1) <- makeReg false

  -- Did instruction in execute stage request a retry on previous cycle?
  doRetry :: Reg (Bit 1) <- makeDReg false

  -- Program counters for each pipeline stage
  pc1 :: Reg (Bit 32) <- makeReg 0xfffffffc
  pc2 :: Reg (Bit 32) <- makeReg dontCare
  pc3 :: Reg (Bit 32) <- makeReg dontCare

  -- Instruction registers for pipeline stages 2, 3 and 4
  instr2 :: Reg Instr <- makeReg 0
  instr3 :: Reg Instr <- makeReg 0
  instr4 :: Reg Instr <- makeReg 0

  -- Stage-active registers for pipeline stages 2, 3, and 4
  active2 :: Reg (Bit 1) <- makeReg false
  active3 :: Reg (Bit 1) <- makeReg false

  always do
    -- Stage 0: Instruction Fetch
    -- ==========================

    -- PC to fetch
    let pcFetch = pcNext.active ? (pcNext.val, pc1.val + 4)

    -- Index the instruction memory
    let instrAddr = lower (slice @31 @2 pcFetch)
    load instrMem instrAddr

    -- Handle stall
    if stallWire.val
      then instrMem.preserveOut
      else pc1 <== pcFetch

    -- Stage 1 always active, except on first cycle
    let active1 :: Bit 1 = reg 0 1

    -- Stage 1: Operand Fetch
    -- ======================

    if pcNext.active
      then do
        -- Disable stage 2 on pipeline flush
        active2 <== false
      else do
        -- Move to stage 2 when not stalling
        when (stallWire.val.inv) do
          active2 <== active1
          instr2  <== instrMem.out
          pc2     <== pc1.val

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

    when (stallWire.val.inv) do
      -- Latch operands
      regA <== a
      regB <== b
      regBorImm <== bOrImm

      -- Latch instruction and PC for next stage
      instr3 <== instr2.val
      pc3 <== pc2.val

    -- Stall on back-pressure from memory subsystem
    when (memReqQueue.notFull.inv) do
      stallWire <== true

    -- Enable execute stage?
    if pcNext.active
      then do
        -- Disable stage 3 on pipeline flush
        active3 <== false
      else do
        -- Enable stage 3 when there's no pipeline stall
        active3 <== active2.val .&. stallWire.val.inv

    -- Stage 3: Execute
    -- ================

    -- Buffer the decode tables
    let bufferEn = delayEn dontCare
    let bufferField en opt =
          Option (bufferEn en (opt.valid)) (map (bufferEn en) (opt.val))
    let tagMap3 = Map.map (bufferEn (active2.val)) tagMap
    let fieldMap3 = Map.map (bufferField (active2.val)) fieldMap

    -- State for execute stage
    let state = State {
            instr = instr3.val
          , opA = regA.val
          , opB = regB.val
          , opBorImm = regBorImm.val
          , pc = ReadWrite (pc3.val) (pcNext <==)
          , result = WriteOnly $ \x ->
                       when (instr3.val.dst .!=. 0) do
                         resultWire <== x
          , memLoad = \a -> do
              let req = MemReq {
                          memReqIsStore = false
                        , memReqAddr    = a
                        , memReqByteEn  = dontCare
                        , memReqData    = dontCare
                        }
              enq memReqQueue req
              memReqInFlight <== true
              -- In future, we could allow independent instructions to
              -- bypass multi-cycle load instructions
              stallWire <== true
          , memStore = \a be d -> do
              let req = MemReq {
                          memReqIsStore = true
                        , memReqAddr    = a
                        , memReqByteEn  = be
                        , memReqData    = d
                        }
              enq memReqQueue req
          , suspend = do
              suspendInProgress <== true
              stallWire <== true
              return dontCare
          , retry = do
              doRetry <== true
              stallWire <== true
          , opcode = tagMap3
          , fields = fieldMap3
          }

    -- Execute stage
    when (active3.val .|. doRetry.val) do
      executeStage c state
    when (active3.val) do
      instr4 <== instr3.val

    -- Stage 4: Writeback
    -- ==================

    -- Buffer the decode tables
    let tagMap4 = Map.map (bufferEn (active3.val)) tagMap3
    let fieldMap4 = Map.map (bufferField (active3.val)) fieldMap3

    -- State for memory response stage
    let state = State {
            instr = instr4.val
          , opA = regA.val.old
          , opB = regB.val.old
          , opBorImm = regBorImm.val.old
          , pc = error "Can't access PC in memory response stage"
          , result = WriteOnly $ \x ->
                       when (instr4.val.dst .!=. 0) do
                         memRespResultWire <== x
          , memLoad = error "Can't issue load in memory response stage"
          , memStore = error "Can't issue store in memory response stage"
          , suspend = error "Can't resume in memory response stage"
          , retry = error "Can't retry in memory response stage"
          , opcode = tagMap4
          , fields = fieldMap4
          }

    -- Memory response / resume stage
    if memResps.canPeek
      then do
        memResps.consume
        memResponseStage c state (memResps.peek)
        memReqInFlight <== false
      else do
        let resumeReqs = c.resumeStage
        if resumeReqs.canPeek
          then do
            let resumeReq = resumeReqs.peek
            resumeReqs.consume
            when (instr4.val.dst .!=. 0) do
              memRespResultWire <== resumeReqs.peek.resumeReqData
            suspendInProgress <== false
          else do
            -- Stall while waiting for response
            when (memReqInFlight.val .|. suspendInProgress.val) do
              stallWire <== true

    -- Determine final result
    let rd = instr4.val.dst
    when (memRespResultWire.active) do
      finalResultWire <== memRespResultWire.val
    when (memRespResultWire.active.inv .&. delay 0 (resultWire.active)) do
      finalResultWire <== resultWire.val.old

    -- Writeback
    when (finalResultWire.active) do
      store regFileA rd (finalResultWire.val)
      store regFileB rd (finalResultWire.val)

    return (memReqQueue.toStream)
