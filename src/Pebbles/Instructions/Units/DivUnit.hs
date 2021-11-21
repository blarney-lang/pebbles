module Pebbles.Instructions.Units.DivUnit
  ( DivReq(..)
  , DivUnit
  , VecDivUnit
  , makeSeqVecDivUnit
  , makeSeqDivUnit
  , makeFullVecDivUnit
  , makeFullDivUnit
  ) where

-- Blarney imports
import Blarney
import Blarney.Stmt
import Blarney.Queue
import Blarney.Option
import Blarney.SourceSink
import Blarney.ClientServer
import Blarney.Vector (Vec, toList, fromList)
import Blarney.Vector qualified as V
import Blarney.VendorIP.PipelinedDivider

-- Pebbles imports
import Pebbles.Util.List
import Pebbles.Util.Counter
import Pebbles.Pipeline.Interface

-- Haskell imports
import Data.List

-- | Request to divider unit
data DivReq =
  DivReq {
    divReqNum :: Bit 32
  , divReqDenom :: Bit 32
    -- ^ Numerator and denominator
  , divReqIsSigned :: Bit 1
    -- ^ Signed or unsigned division?
  , divReqGetRemainder :: Bit 1
    -- ^ Do we want the quotient or remainder?
  } deriving (Generic, Interface, Bits)

-- | Scalar divider unit interface
type DivUnit t_id =
  Server (t_id, DivReq)
         (t_id, ResumeReq)

-- | Vector divider unit interface
type VecDivUnit n t_id =
  Server (t_id, Vec n (Option DivReq))
         (t_id, Vec n (Option ResumeReq))

-- Sequential divider
-- ==================

-- | Sequential divider state
data SeqDivState =
  SeqDivState {
    n            :: Reg (Bit 32) -- ^ Numerator
  , d            :: Reg (Bit 32) -- ^ Denominator
  , q            :: Reg (Bit 32) -- ^ Quotient
  , r            :: Reg (Bit 32) -- ^ Remainder
  , isSigned     :: Reg (Bit 1)  -- ^ Signed or unsigned division?
  , getRemainder :: Reg (Bit 1)  -- ^ Quotient or remainder?
  , negResult    :: Reg (Bit 1)  -- ^ Flip the sign of the result?
  , output       :: Reg (Bit 32) -- ^ Output register
  }

-- | Create sequential divider state
makeSeqDivState :: Module SeqDivState
makeSeqDivState =
      SeqDivState
  <$> makeReg dontCare
  <*> makeReg dontCare
  <*> makeReg dontCare
  <*> makeReg dontCare
  <*> makeReg dontCare
  <*> makeReg dontCare
  <*> makeReg dontCare
  <*> makeReg dontCare

-- | Vector divider unit (sequential state machine version)
makeSeqVecDivUnit :: forall n t_id.
      (KnownNat n, Bits t_id)
   => Module (VecDivUnit n t_id)
makeSeqVecDivUnit = do
  let vecSize = valueOf @n

  -- Counter
  count :: Reg (Bit 6) <- makeReg dontCare

  -- Is the result ready for consumption?
  done :: Reg (Bit 1) <- makeReg false

  -- Is the result currently being computed?
  busy :: Reg (Bit 1) <- makeReg false

  -- Trigger division state machine
  trigger :: Reg (Bit 1) <- makeDReg false

  -- Id of current request
  idReg :: Reg t_id <- makeReg dontCare

  -- Per-lane state
  laneStates :: [SeqDivState] <- replicateM vecSize makeSeqDivState

  -- Active lanes
  active :: Reg (Bit n) <- makeReg dontCare

  -- Helper function to shift left by one
  let shl x = x .<<. (1 :: Bit 1)

  -- State machine
  runStmtOn (trigger.val) do
    -- Prepare division
    action do
      count <== 32
      sequence_
        [ do let divByZero = s.d.val .==. 0
             let n_msb = at @31 (s.n.val)
             let d_msb = at @31 (s.d.val)
             -- Negate numerator and denominator if required
             when (s.isSigned.val .&. inv divByZero) do
               when n_msb do s.n <== negate s.n.val
               when d_msb do s.d <== negate s.d.val
             -- Initialise state machine
             s.q <== 0
             s.r <== 0
             -- Negate result?
             s.negResult <== s.isSigned.val .&. inv divByZero .&.
               (s.getRemainder.val ? (n_msb, n_msb .^. d_msb))
        | s <- laneStates ]
    -- Binary long division algorithm (taken from Wikipedia)
    while (count.val .!=. 0) do
      action do
        count <== count.val - 1
        sequence_
          [ do let r' = shl s.r.val .|. zeroExtend (at @31 (s.n.val))
               let sub = r' .>=. s.d.val
               s.r <== r' - (sub ? (s.d.val, 0))
               s.q <== shl s.q.val .|. (sub ? (1, 0))
               s.n <== shl s.n.val
          | s <- laneStates ]
    -- Prepare result
    action do
      done <== true
      busy <== false
      sequence_
        [ do -- Choose quotient or remainder, and optionally negate
             let result = s.getRemainder.val ? (s.r.val, s.q.val)
             s.output <== s.negResult.val ? (negate result, result)
        | s <- laneStates ]

  return
    Server {
      reqs =
        Sink {
          canPut = inv busy.val .&&. inv done.val
        , put = \(id, reqVec) -> do
            busy <== true
            trigger <== true
            active <== fromBitList (map (.valid) (toList reqVec))
            idReg <== id
            sequence_
              [ do s.n <== req.val.divReqNum
                   s.d <== req.val.divReqDenom
                   s.isSigned <== req.val.divReqIsSigned
                   s.getRemainder <== req.val.divReqGetRemainder
              | (req, s) <- zip (toList reqVec) laneStates ]
        }
    , resps =
        Source {
          canPeek = done.val
        , peek =
            ( idReg.val
            , fromList
                [ Option valid
                    ResumeReq {
                      resumeReqData = s.output.val
                    , resumeReqCap  = none
                    }
                | (s, valid) <- zip laneStates (toBitList active.val) ] )
        , consume = do done <== false
        }
    }

-- | Scalar divider unit (sequential state machine version)
makeSeqDivUnit :: Bits t_id => Module (DivUnit t_id)
makeSeqDivUnit = do
  vecDivUnit <- makeSeqVecDivUnit @1
  return
    Server {
      reqs  = mapSink (\(id, req) -> (id, fromList [Option true req]))
                      vecDivUnit.reqs
    , resps = mapSource (\(id, resp) -> (id, (head (toList resp)).val))
                        vecDivUnit.resps
    }

-- Full-throughput divider
-- =======================

-- | Full throughput vector divider unit (using Blarney IP)
makeFullVecDivUnit :: forall n t_id.
     (KnownNat n, Bits t_id)
  => Int
  -> Module (VecDivUnit n t_id)
makeFullVecDivUnit latency
    -- Check that latency is small enough to allow result queue to fit in MLABs
  | latency >= 32 = error "makeFullVecDivUnit: latency too large"
  | otherwise = do
      let vecSize = valueOf @n

      -- Result and request active/id queues
      resultQueue :: Queue (Vec n (Option (Bit 32))) <- makeSizedQueue 5
      infoQueue :: Queue t_id <- makeSizedQueue 5

      -- Wire indicating request insertion
      reqInsertWire :: Wire (Bit 1) <- makeWire false

      -- Per-lane wires (numerator, denominator, want remainder)
      numWires   :: [Wire (Bit 33)] <- replicateM vecSize $ makeWire dontCare
      denomWires :: [Wire (Bit 33)] <- replicateM vecSize $ makeWire dontCare
      remWires   :: [Wire (Bit 1)]  <- replicateM vecSize $ makeWire dontCare

      always do
        -- Output of pipelined divider megafunction
        let (quots, rems) = unzip $
              zipWith (pipelinedDivider latency)
                      (map (.val) numWires)
                      (map (.val) denomWires)
        -- Wait 'latency' cycles for result to be ready
        let ready = iterateN latency (delay false) reqInsertWire.val
        let actives =
              [ iterateN latency (delay false) remWire.active
              | remWire <- remWires ]
        let wantRems =
              [ iterateN latency (delay false) remWire.val
              | remWire <- remWires ]
        let divByZeros =
              [ iterateN latency (delay false) (denomWire.val .==. 0)
              | denomWire <- denomWires ]
        -- Fill result queue
        when ready do
          -- Result queue must have space
          dynamicAssert (resultQueue.notFull)
            "FullVecDivUnit: result queue overflow"
          -- Enqueue result
          resultQueue.enq $
            fromList 
              [ Option valid $
                  truncate $
                    if wantRem
                      then rem
                      else divByZero ? (ones, quot)
              | (quot, rem, valid, wantRem, divByZero) <-
                  zip5 quots rems actives wantRems divByZeros
              ]

      return
        Server {
          reqs =
            Sink {
              canPut = infoQueue.notFull
            , put = \(id, reqVec) -> do
                infoQueue.enq id
                -- Assign wires, per lane
                sequence_
                  [ do -- Extend inputs to 33 bits
                       let numExt = req.val.divReqIsSigned ?
                                      (upper req.val.divReqNum, false)
                       let denomExt = req.val.divReqIsSigned ?
                                      (upper req.val.divReqDenom, false)
                       numWire <== numExt # req.val.divReqNum
                       denomWire <== denomExt # req.val.divReqDenom
                       when req.valid do
                         remWire <== req.val.divReqGetRemainder
                  | (req, numWire, denomWire, remWire) <-
                      zip4 (toList reqVec) numWires denomWires remWires ]
                -- Insert request
                reqInsertWire <== true
            }
        , resps =
            Source {
              canPeek = resultQueue.canDeq .&&. infoQueue.canDeq
            , peek =
                ( infoQueue.first
                , fromList [ fmap (\d -> ResumeReq {
                                           resumeReqCap = none
                                         , resumeReqData = d
                                         } ) res
                           | res <- toList resultQueue.first ] )
            , consume = do
                resultQueue.deq
                infoQueue.deq
            }
        }

-- | Full throughput scalar divider unit (using Blarney IP)
makeFullDivUnit :: Bits t_id => Int -> Module (DivUnit t_id)
makeFullDivUnit latency = do
  vecDivUnit <- makeFullVecDivUnit @1 latency
  return
    Server {
      reqs  = mapSink (\(id, req) -> (id, fromList [Option true req]))
                      vecDivUnit.reqs
    , resps = mapSource (\(id, resp) -> (id, (head (toList resp)).val))
                        vecDivUnit.resps
    }
