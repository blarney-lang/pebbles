module Pebbles.Memory.WarpPreserver where

-- SoC parameters
#include <SoC.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.SourceSink
import qualified Blarney.Vector as V

-- Pebbles imports
import Pebbles.Memory.Interface

-- Haskell imports
import Data.List

-- | It can be desirable to keep memory requests from the same warp
-- together as they enter the memory subsytem.  This is because these
-- requests are likely to exhibit locality with respect to each other,
-- which helps the coalescing unit.  If only some SIMT lanes make a
-- request on a given clock cycle (due to divergence), then requests
-- form different warps could get mixed up, which could hinder
-- coalescing.  The following module keeps requests from the same warp
-- together, moving from warp to warp only when all requests have
-- been consumed.
makeWarpPreserver :: Bits t_id =>
     -- | Memory response stream to each SIMT lane
     [Stream (MemResp t_id)]
     -- | A memory unit interface for each SIMT lane and a
     --   memory request stream for each SIMT lane
  -> Module ([Stream (MemReq t_id)], [MemUnit t_id])
makeWarpPreserver resps = do
  -- Queue of vectorised memory requests
  memReqsQueue :: Queue (V.Vec SIMTLanes (MemReq t_id)) <-
    makeShiftQueue 1

  -- Queue of masks indicating which vector elements are valid
  validMask :: Queue (Bit SIMTLanes) <- makeShiftQueue 1

  -- Wires indicating if each lane putting or not 
  putWires :: [Wire (MemReq t_id)] <- replicateM SIMTLanes (makeWire dontCare)

  -- Wires indicating if each lane is consuming or not 
  getWires :: [Wire (Bit 1)] <- replicateM SIMTLanes (makeWire false)

  -- Register tracking requests at the head of the queue that have
  -- already been consumed
  gotMask :: Reg (Bit SIMTLanes) <- makeReg 0

  -- Catch case when a request is being enqueued
  let anyPut = orList (map active putWires)

  -- Fill queues
  always do
    when anyPut do
      dynamicAssert (memReqsQueue.notFull .&. validMask.notFull)
        "WarpPreserver: overflow"
      enq memReqsQueue (V.fromList $ map val putWires)
      enq validMask (fromBitList $ map active putWires)

  -- Drain queues
  always do
    when (memReqsQueue.canDeq) do
      dynamicAssert (validMask.canDeq) "WarpPreserver: mismatch"
      let mask = validMask.first
      let getMask = fromBitList (map val getWires)
      let remaining = (mask .&. getMask.inv) .&. gotMask.val.inv
      -- Dequeue when all requests have been consumed
      if remaining .==. 0
        then do
          validMask.deq
          memReqsQueue.deq
          gotMask <== 0
        else do
          gotMask <== gotMask.val .|. getMask

  -- Ouputs
  let reqStreams =
        [ Source {
            peek = req
          , canPeek = memReqsQueue.canDeq .&. valid .&. inv got
          , consume = getWire <== true
          }
        | (req, valid, got, getWire) <-
            zip4 (V.toList $ memReqsQueue.first)
                 (toBitList $ validMask.first)
                 (toBitList $ gotMask.val)
                 getWires
        ]

  let memUnits =
        [ MemUnit {
            memReqs =
              Sink {
                canPut = memReqsQueue.notFull
              , put = \x -> putWire <== x
              }
          , memResps = resp
          }
        | (resp, putWire) <- zip resps putWires
        ]

  return (reqStreams, memUnits)
