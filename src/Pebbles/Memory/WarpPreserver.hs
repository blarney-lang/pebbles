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

  -- Mask on input lanes to deal with multi-flit transactions
  mask :: [Reg (Bit 1)] <- replicateM SIMTLanes (makeReg true)

  -- Catch case when a request is being enqueued
  let anyPut = orList (map active putWires)

  -- Is any request a multi-flit transaction?
  let anyTransaction =
        orList [p.active .&&. p.val.memReqIsFinal.inv | p <- putWires]

  -- Fill queues
  always do
    when anyPut do
      dynamicAssert (memReqsQueue.notFull)
        "WarpPreserver: overflow"
      -- Update queue
      enq memReqsQueue $ V.fromList $
        [Option (p.active) (p.val) | p <- putWires]
      -- Update mask
      if anyTransaction
        then do
          sequence_
            [ maskReg <== p.active
            | (maskReg, p) <- zip mask putWires ]
        else do
          sequence_ [r <== true | r <- mask]

  -- Ouputs
  let reqStreams = memReqsQueue.toStream

  let memUnits =
        [ MemUnit {
            memReqs =
              Sink {
                canPut = memReqsQueue.notFull .&&. maskReg.val
              , put = \x -> putWire <== x
              }
          , memResps = resp
          }
        | (resp, putWire, maskReg) <- zip3 resps putWires mask
        ]

  return (reqStreams, memUnits)
