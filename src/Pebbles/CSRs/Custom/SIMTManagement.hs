-- Custom CSRs for managing the SIMT core from the CPU
module Pebbles.CSRs.Custom.SIMTManagement where

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream

-- Pebbles imports
import Pebbles.CSRs.CSRUnit
import Pebbles.Pipeline.SIMT.Management

-- +------------------+---------+--------+-----------------------------------+
-- | CSR              | Address | Access | Description                       |
-- +------------------+---------+--------+-----------------------------------+
-- | SIMTCanPut       |   0x820 | R      | Can issue SIMT request?           |
-- | SIMTInstrAddr    |   0x821 | W      | Set instruction mem address       |
-- | SIMTWriteInstr   |   0x822 | W      | Write to instruction mem          |
-- | SIMTStartKernel  |   0x823 | W      | Start all warps with given PC     |
-- | SIMTCanGet       |   0x824 | R      | Can get SIMT response?            |
-- | SIMTGet          |   0x825 | R      | Get SIMT response                 |
-- +------------------+---------+--------+-----------------------------------+

-- Notes: SIMTWriteInstr and SIMTStartKernel must only be accessed if
-- SIMTCanPut is true.  Similarly, SIMTGet must only be accessed if
-- SIMTCanGet is true.

-- | CSRs for management of SIMT core from CPU
makeCSRs_SIMTManagement ::
     -- | Responses from SIMT core
     Stream SIMTResp
     -- | Requests to SIMT core, and a list of CSRs
  -> Module (Stream SIMTReq, [CSR])
makeCSRs_SIMTManagement resps = do
  -- Queue of requests to SIMT core
  reqs :: Queue SIMTReq <- makeShiftQueue 1

  -- Address for instruction memory write
  addrReg :: Reg (Bit 32) <- makeReg dontCare

  -- Check if command can be issued to SIMT core
  let csr_SIMTCanPut =
        CSR {
          csrId = 0x820
        , csrRead = Just do return (reqs.notFull.zeroExtend)
        , csrWrite = Nothing
        }
 
  -- Set address for instruction memory write
  let csr_SIMTInstrAddr =
        CSR {
          csrId = 0x821
        , csrRead = Nothing
        , csrWrite = Just \x -> do
            addrReg <== x
        }

  -- Write to instruction memory
  let csr_SIMTWriteInstr =
        CSR {
          csrId = 0x822
        , csrRead = Nothing
        , csrWrite = Just \x -> do
            dynamicAssert (reqs.notFull)
              "SIMTWriteInstr CSR: SIMTCanPut not checked"
            when (reqs.notFull) do
              enq reqs
                SIMTReq {
                  simtReqCmd  = simtCmd_WriteInstr
                , simtReqAddr = addrReg.val
                , simtReqData = x
                }
        }

  -- Write to instruction memory
  let csr_SIMTStartKernel =
        CSR {
          csrId = 0x823
        , csrRead = Nothing
        , csrWrite = Just \x -> do
            dynamicAssert (reqs.notFull)
              "SIMTWriteInstr CSR: SIMTCanPut not checked"
            enq reqs
              SIMTReq {
                simtReqCmd  = simtCmd_StartPipeline
              , simtReqAddr = x
              , simtReqData = dontCare
              }
        }

  -- Check if response can be received from SIMT core
  let csr_SIMTCanGet =
        CSR {
          csrId = 0x824
        , csrRead = Just do return (resps.canPeek.zeroExtend)
        , csrWrite = Nothing
        }

  -- Receive response from SIMT core
  let csr_SIMTGet =
        CSR {
          csrId = 0x825
        , csrRead = Just do
            dynamicAssert (resps.canPeek)
              "SIMTGet CSR: SIMTCanGet not checked"
            resps.consume
            return (resps.peek.zeroExtend)
        , csrWrite = Nothing
        }


  let csrs =
        [ csr_SIMTCanPut
        , csr_SIMTInstrAddr
        , csr_SIMTWriteInstr
        , csr_SIMTStartKernel
        , csr_SIMTCanGet
        , csr_SIMTGet
        ]

  return (reqs.toStream, csrs)