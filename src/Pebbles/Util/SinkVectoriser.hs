module Pebbles.Util.SinkVectoriser where

-- SoC parameters
#include <Config.h>

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.Option
import Blarney.SourceSink
import Blarney.Vector (Vec, fromList, toList)

-- | Convert a sink of request vectors to a vector of request sinks.
-- The request id is sampled at the point at which requests are
-- submitted to one or more sinks.
makeSinkVectoriser :: forall n t_put req.
     (KnownNat n, Bits t_put, Bits req)
     -- ^ Constraints
  => (Vec n (Option req) -> t_put)
     -- ^ Convert request vector to value to be put to sink
  -> Sink t_put
     -- ^ Sinks after vectorisation
  -> Module (Vec n (Sink req))
     -- ^ Sinks before vectorisation
makeSinkVectoriser f sink = do
  let vecSize = valueOf @n

  -- Wires indicating if each lane putting or not 
  putWires :: [Wire req] <- replicateM vecSize (makeWire dontCare)

  always do
    let anyPut = orList $ map (.active) putWires
    let vec = fromList [Option w.active w.val | w <- putWires]
    when anyPut do sink.put (f vec)

  return $ fromList
    [ Sink {
         canPut = sink.canPut
       , put = \req -> do putWire <== req
      }
    | putWire <- putWires ]
