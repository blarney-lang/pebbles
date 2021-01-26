-- Custom CSRs for warp control on the SIMT core
module Pebbles.CSRs.Custom.WarpControl where

-- Blarney imports
import Blarney

-- Pebbles imports
import Pebbles.CSRs.CSRUnit

-- +------------------+---------+--------+-----------------------------------+
-- | CSR              | Address | Access | Description                       |
-- +------------------+---------+--------+-----------------------------------+
-- | WarpTerminate    |   0x830 | W      | Terminate current warp            |
-- | WarpGetKernel    |   0x831 | R      | Get address of kernel closure     |
-- +------------------+---------+--------+-----------------------------------+

-- Notes: The value written to WarpTerminate is current treated as a
-- boolean, indicating the success or failure of the warp.  The
-- write assumes that all threads in the warp have converged and
-- indicates that the warp should be terminated.

-- | CSRs for management of SIMT core from CPU
makeCSRs_WarpControl :: Bit 32 -> Module (Wire (Bit 1), [CSR])
makeCSRs_WarpControl kernelAddr = do
  -- A pulse on this wire indicates termination
  termWire :: Wire (Bit 1) <- makeWire 0

  -- Terminate warp
  let csr_WarpTerminate =
        CSR {
          csrId = 0x830
        , csrRead = Nothing
        , csrWrite = Just \x -> do
            termWire <== x.truncate
        }

  -- Get kernel closure
  let csr_WarpGetKernel =
        CSR {
          csrId = 0x831
        , csrRead = Just do return kernelAddr
        , csrWrite = Nothing
        }

  return (termWire, [csr_WarpTerminate, csr_WarpGetKernel])
