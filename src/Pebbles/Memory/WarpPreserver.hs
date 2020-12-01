module Pebbles.Memory.WarpPreserver where

-- Blarney imports
import Blarney
import Blarney.SourceSink

-- Pebbles imports
import Pebbles.Memory.Interface

-- | It can be desirable to keep memory requests from the same warp
-- together as they enter the memory subsytem.  This is because these
-- requests are likely to exhibit locality with respect to each other,
-- which helps the coalescing unit.  If only some SIMT lanes make a
-- request on a given clock cycle (due to divergence), then requests
-- form different warps could get mixed up, which could hinder
-- coalescing.  The following module resolves this by implciclity
-- inserting "null" requests from the SIMT lanes than do not make a
-- request on the same cycle as those that do.
makeWarpPreserver :: Bits t_id =>
     -- | A memory request sink for each SIMT lane
     [Sink (MemReq t_id)]
     -- | A memory request sink for each SIMT lane, with null requests
     -- inserted to keep the requests of the same warp in lock-step
  -> Module [Sink (MemReq t_id)]
makeWarpPreserver inps = do
  -- Determine if all sinks allow put
  let allCanPut = tree (.&.) true [inp.canPut | inp <- inps]

  -- Null request
  let nullReq = dontCare { memReqOp = memNullOp }

  -- Boolean wire indicating if each lane is putting or not
  putWires :: [Wire (Bit 1)] <- replicateM (length inps) (makeWire false)

  -- Catch case when a request is being made to any sink
  let anyPut = tree (.|.) false (map val putWires)

  always do
    -- Insert null requests
    forM_ (zip inps putWires) \(inp, putWire) ->
      when (anyPut .&. putWire.val.inv) do
        put inp nullReq

  return
    [ Sink {
        -- Can only put if all sinks can put
        canPut = allCanPut
        -- Observe when put is called
      , put = \x -> do
          put inp x
          putWire <== true
      }
      | (inp, putWire) <- zip inps putWires
    ]
