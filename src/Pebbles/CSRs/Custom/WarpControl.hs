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
-- +------------------+---------+--------+-----------------------------------+

-- Notes: The value written to WarpTerminate is currently unused.  The
-- write assumes that all threads in the warp have converged and
-- indicates that the warp should be terminated.

-- | CSRs for management of SIMT core from CPU
makeCSRs_WarpControl :: Module (Wire (Bit 1), [CSR])
makeCSRs_WarpControl = do
  -- A pulse on this wire indicates termination
  termWire :: Wire (Bit 1) <- makeWire 0

  -- Check if command can be issued to SIMT core
  let csr_WarpTerminate =
        CSR {
          csrId = 0x830
        , csrRead = Nothing
        , csrWrite = Just \x -> do
            termWire <== x.truncate
        }

  return (termWire, [csr_WarpTerminate])
