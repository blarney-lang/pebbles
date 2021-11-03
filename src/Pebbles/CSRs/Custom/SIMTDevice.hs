-- Custom CSRs for warp control on the SIMT core
module Pebbles.CSRs.Custom.SIMTDevice where

-- Blarney imports
import Blarney

-- Pebbles imports
import Pebbles.CSRs.CSRUnit

-- +------------------+---------+--------+-----------------------------------+
-- | CSR              | Address | Access | Description                       |
-- +------------------+---------+--------+-----------------------------------+
-- | WarpCmd          |   0x830 | W      | Warp command register             |
-- | WarpGetKernel    |   0x831 | R      | Get address of kernel closure     |
-- +------------------+---------+--------+-----------------------------------+

-- A write to WarpCmd assumes that all threads in the warp have converged

-- | Warp command
data WarpCmd =
  WarpCmd {
    warpCmd_termCode :: Bit 1
    -- ^ The termination code
  , warpCmd_isTerminate :: Bit 1
    -- ^ Is it a termination or barrier command?
  }
  deriving (Generic, Bits, Interface)

-- | CSR for warp commands
makeCSR_WarpCmd :: KnownNat n =>
  Bit n -> Wire WarpCmd -> Module CSR
makeCSR_WarpCmd laneId warpCmd = do
  let csr_WarpCmd =
        CSR {
          csrId = 0x830
        , csrRead = Nothing
        , csrWrite = Just \x -> do
            when (laneId .==. 0) do
              warpCmd <== unpack (truncate x)
        }

  return csr_WarpCmd

-- | CSR for accessing kernel closure address
makeCSR_WarpGetKernel :: Bit 32 -> Module CSR
makeCSR_WarpGetKernel kernelAddr = do
  -- Get kernel closure
  let csr_WarpGetKernel =
        CSR {
          csrId = 0x831
        , csrRead = Just do return kernelAddr
        , csrWrite = Nothing
        }

  return csr_WarpGetKernel
