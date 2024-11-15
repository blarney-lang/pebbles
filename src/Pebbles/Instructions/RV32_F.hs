-- Support for the floating-point (Zfinx) extension

module Pebbles.Instructions.RV32_F
  ( decodeF
  , executeF
  , toFPUOpcode
  ) where

-- Blarney imports
import Blarney
import Blarney.Stmt
import Blarney.Queue
import Blarney.Stream
import Blarney.BitScan
import Blarney.SourceSink
import Blarney.TaggedUnion hiding (is)

-- Pebbles imports
import Pebbles.CSRs.CSRUnit
import Pebbles.CSRs.TrapCodes
import Pebbles.Memory.Interface
import Pebbles.Pipeline.Interface
import Pebbles.Instructions.Mnemonics
import Pebbles.Instructions.Units.FPU
import Pebbles.Instructions.Units.SFU

-- Haskell imports
import Data.Maybe

-- Decode stage
-- ============

decodeF =
  [ "0000000 rs2<5> rs1<5> rm<3> rd<5> 1010011" --> FADD
  , "0000100 rs2<5> rs1<5> rm<3> rd<5> 1010011" --> FSUB
  , "0001000 rs2<5> rs1<5> rm<3> rd<5> 1010011" --> FMUL
  , "0001100 rs2<5> rs1<5> rm<3> rd<5> 1010011" --> FDIV
  , "0101100 00000  rs1<5> rm<3> rd<5> 1010011" --> FSQRT
  , "0010100 rs2<5> rs1<5> 000   rd<5> 1010011" --> FMIN
  , "0010100 rs2<5> rs1<5> 001   rd<5> 1010011" --> FMAX
  , "1100000 00000  rs1<5> rm<3> rd<5> 1010011" --> FCVT_W_S
  , "1100000 00001  rs1<5> rm<3> rd<5> 1010011" --> FCVT_WU_S
  , "1101000 00000  rs1<5> rm<3> rd<5> 1010011" --> FCVT_S_W
  , "1101000 00001  rs1<5> rm<3> rd<5> 1010011" --> FCVT_S_WU
  , "1010000 rs2<5> rs1<5> 010   rd<5> 1010011" --> FEQ
  , "1010000 rs2<5> rs1<5> 001   rd<5> 1010011" --> FLT
  , "1010000 rs2<5> rs1<5> 000   rd<5> 1010011" --> FLE
  , "0010000 rs2<5> rs1<5> 000   rd<5> 1010011" --> FSGNJ_S
  , "0010000 rs2<5> rs1<5> 001   rd<5> 1010011" --> FSGNJN_S
  , "0010000 rs2<5> rs1<5> 010   rd<5> 1010011" --> FSGNJX_S
  ]

-- Field selectors
-- ===============

getRoundingMode :: Bit 32 -> Bit 3
getRoundingMode = makeFieldSelector decodeF "rm"

-- FPU opcode
-- ==========

toFPUOpcode :: MnemonicVec -> FPUOpcode
toFPUOpcode i =
  select [
    i `is` [FADD, FSUB]           --> fpuAddSubOp
  , i `is` [FMUL]                 --> fpuMulOp
  , i `is` [FDIV]                 --> fpuDivOp
  , i `is` [FSQRT]                --> fpuSqrtOp
  , i `is` [FMIN]                 --> fpuMinOp
  , i `is` [FMAX]                 --> fpuMaxOp
  , i `is` [FCVT_W_S]             --> fpuToIntOp
  , i `is` [FCVT_WU_S]            --> fpuToUIntOp
  , i `is` [FCVT_S_W, FCVT_S_WU]  --> fpuFromIntOp
  , i `is` [FEQ]                  --> fpuEQOp
  , i `is` [FLT]                  --> fpuLTOp
  , i `is` [FLE]                  --> fpuLEOp
  ]

-- Execute stage
-- =============

executeF ::
     Sink FPUReq
     -- ^ Access to floating-point unit
  -> Maybe (Sink SFUReq)
     -- ^ Access to SFU
  -> State
     -- ^ Pipeline state
  -> Action ()
executeF fpu mb_sfu s = do

  -- Opcodes of floating-point operations except FDIV and FSQRT
  let opcode = [FADD, FSUB, FMUL, FMIN, FMAX,
                  FCVT_W_S, FCVT_WU_S, FCVT_S_W, FCVT_S_WU,
                    FEQ, FLT, FLE]

  -- Opcodes of floating-point operations implemented per lane
  let perLaneOp = opcode ++ (if isNothing mb_sfu then [FDIV, FSQRT] else [])

  -- Send most instructions to the FPU
  when (s.opcode `is` perLaneOp) do
    if fpu.canPut
      then do
        s.suspend
        fpu.put
          FPUReq {
            opcode = toFPUOpcode s.opcode
          , opA    = s.opA
          , opB    = s.opB
          , ctrl   = select [
                       s.opcode `is` [FADD] --> true
                     , s.opcode `is` [FCVT_S_WU] --> true
                     ]
          } 
      else do
        s.retry

  -- Sign-injection instructions
  when (s.opcode `is` [FSGNJ_S, FSGNJN_S, FSGNJX_S]) do
    let newSign = select [
                    s.opcode `is` [FSGNJ_S]  --> at @31 s.opB
                  , s.opcode `is` [FSGNJN_S] --> inv (at @31 s.opB)
                  , s.opcode `is` [FSGNJX_S] --> at @31 s.opA .^. at @31 s.opB
                  ]
    s.result <== (newSign # lower s.opA)

  -- Send some instructions to SFU
  case mb_sfu of
    Nothing -> return ()
    Just sfu -> do
      when (s.opcode `is` [FDIV, FSQRT]) do
        if sfu.canPut
          then do
            s.suspend
            sfu.put
              SFUReq {
                kind = tag #fp SFUFPReq {
                                 isDiv = s.opcode `is` [FDIV]
                               }
              , opA = s.opA
              , opB = s.opB
              , capA = dontCare
              }
          else do
            s.retry
