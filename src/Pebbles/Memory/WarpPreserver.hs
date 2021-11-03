module Pebbles.Memory.WarpPreserver where

-- SoC parameters
#include <Config.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.Option
import Blarney.SourceSink
import qualified Blarney.Vector as V

-- Pebbles imports
import Pebbles.Memory.Interface

-- Haskell imports
import Data.List

-- | It is desirable to keep memory requests from the same warp
-- together as they enter the memory subsytem.  This is because these
-- requests are likely to exhibit locality with respect to each other,
-- which helps the coalescing unit.  If only some SIMT lanes make a
-- request on a given clock cycle (due to divergence), then requests
-- form different warps could get mixed up, which could hinder
-- coalescing.  The following module keeps requests from the same warp
-- together, moving from warp to warp only when all requests have
-- been consumed.
makeWarpPreserver :: Bits t_id =>
     [Stream (MemResp t_id)]
     -- ^ Memory response stream to each SIMT lane
  -> Module ( Stream (V.Vec SIMTLanes (Option (MemReq t_id)))
            , [MemUnit t_id]
            )
     -- ^ A memory unit interface for each SIMT lane and a
     --   stream of memory request vectors
makeWarpPreserver resps = do
  -- Queue of vectorised memory requests
  memReqsQueue :: Queue (V.Vec SIMTLanes (Option (MemReq t_id))) <-
    makeShiftQueue 1

  -- Wires indicating if each lane putting or not 
  putWires :: [Wire (MemReq t_id)] <- replicateM SIMTLanes (makeWire dontCare)

  -- On the previous enqueue, which lanes were active?
  prevActive :: [Reg (Bit 1)] <- replicateM SIMTLanes (makeReg false)

  -- Is there a multi-flit transaction in progress?
  transactionInProgress <- makeReg false

  -- Catch case when a request is being enqueued
  let anyPut = orList (map active putWires)

  -- Fill queues
  always do
    when anyPut do
      dynamicAssert (memReqsQueue.notFull)
        "WarpPreserver: overflow"
      enq memReqsQueue $ V.fromList $
        [Option (p.active) (p.val) | p <- putWires]
      -- Remember participating lanes
      sequence_ [prev <== p.active | (prev, p) <- zip prevActive putWires]
      -- Track whether multi-flit transaction is in progress
      transactionInProgress <==
        orList [p.active ? (inv p.val.memReqIsFinal, false) | p <- putWires]

  -- Ouputs
  let reqStreams = toStream memReqsQueue

  let memUnits =
        [ MemUnit {
            memReqs =
              Sink {
                canPut = memReqsQueue.notFull .&&.
                  (transactionInProgress.val ? (prev.val, true))
              , put = \x -> putWire <== x
              }
          , memResps = resp
          }
        | (resp, putWire, prev) <- zip3 resps putWires prevActive
        ]

  return (reqStreams, memUnits)
