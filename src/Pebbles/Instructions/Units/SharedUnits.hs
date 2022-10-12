module Pebbles.Instructions.Units.SharedUnits where

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.Stream
import Blarney.SourceSink
import Blarney.ClientServer
import Blarney.Vector (Vec, fromList, toList)

-- Pebbles imports
import Pebbles.Util.List

-- Haskell imports
import Data.Proxy

-- | Implement an m-element server using a smaller n-element server
-- (i.e. m >= n) by time-multiplexing / sharing.
makeSharedUnits :: forall n m t_id req resp.
     (KnownNat n, KnownNat m, Bits req, Bits resp, Bits t_id, n <= m)
     -- ^ Constraints
  => Server (t_id, Vec n (Option req))
            (t_id, Vec n (Option resp))
     -- ^ Take a server for n requests/responses
  -> Module (Server (t_id, Vec m (Option req))
                    (t_id, Vec m (Option resp)))
     -- ^ Produce a server for m requests/responses, where m >= n
makeSharedUnits server
  | valueOf @n == valueOf @m =
      return
        Server {
          reqs =
            Sink {
              canPut = server.reqs.canPut
            , put = \(id, v) -> server.reqs.put (id, fromList $ toList v)
            }
        , resps =
            Source {
              canPeek = server.resps.canPeek
            , peek = let (id, v) = server.resps.peek in
                       (id, fromList $ toList v)
            , consume = server.resps.consume
            }
        }
  | otherwise =
      liftNat (log2ceil (valueOf @m) + 1) \(_ :: Proxy t_count) -> do

        -- Number of n-sized vectors in m-sized vector
        let numShifts = (valueOf @m + valueOf @n - 1) `div` valueOf @n

        -- Request side
        ---------------

        -- Count the number of n-elem vectors in the shift register
        inCount :: Reg (Bit t_count) <- makeReg 0

        -- Input id register
        inId :: Reg t_id <- makeReg dontCare

        -- Input shift register
        inRegs :: [Reg (Option req)] <-
          replicateM (valueOf @m) (makeReg dontCare)

        always do
          when (inCount.val .!=. 0 .&&. server.reqs.canPut) do
            let gs = groupsOf (valueOf @n) inRegs
            -- Insert group into sink
            server.reqs.put (inId.val, fromList $ map (.val) $ head gs)
            -- Shift groups
            sequence_
              [ sequence_ [ r <== x
                          | (r, x) <- zip g0 (map (.val) g1 ++ repeat none) ]
              | (g0, g1) <- zip gs (drop 1 gs) ]
            -- Decrement size
            inCount <== inCount.val - 1

        -- Response side
        ----------------

        -- Count the number of n-elem vectors in the shift register
        outCount :: Reg (Bit t_count) <- makeReg 0

        -- Output id register
        outId :: Reg t_id <- makeReg dontCare

        -- Output shift register
        outRegs :: [[Reg (Option resp)]] <-
          replicateM numShifts
            (replicateM (valueOf @n) (makeReg dontCare))

        always do
          when (outCount.val .!=. fromIntegral numShifts .&&.
                  server.resps.canPeek) do
            let (id, v) = server.resps.peek
            outId <== id
            -- Insert into shift register
            sequence_
              [ r <== x | (r, x) <- zip (last outRegs) (toList v) ]
            -- Shift groups
            sequence_
              [ sequence_ [r0 <== r1.val | (r0, r1) <- zip g0 g1]
              | (g0, g1) <- zip outRegs (drop 1 outRegs) ]
            -- Increment count
            outCount <== outCount.val + 1
            -- Consume response
            server.resps.consume

        return
          Server {
            reqs =
              Sink {
                canPut = inCount.val .==. 0
              , put = \(id, v) -> do
                  inId <== id
                  sequence_ [r <== e | (r, e) <- zip inRegs (toList v)]
                  inCount <== fromIntegral numShifts
              }
          , resps =
              Source {
                canPeek = outCount.val .==. fromIntegral numShifts
              , peek = (outId.val, fromList $ take (valueOf @m)
                                            $ map (.val)
                                            $ concat outRegs)
              , consume = outCount <== 0
              }
          }
