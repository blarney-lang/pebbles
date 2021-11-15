module Pebbles.Instructions.Units.MulUnit where

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.Stream
import Blarney.SourceSink
import Blarney.ClientServer
import Blarney.Vector (Vec, fromList, toList)

-- Pebbles imports
import Pebbles.Pipeline.Interface

-- Haskell imports
import Control.Applicative

-- Interface
-- =========

-- | Multiplier request
data MulReq =
  MulReq {
    -- Operands to multiply
    mulReqA :: Bit 32
  , mulReqB :: Bit 32
    -- Do we want the lower (or upper) bits of the result?
  , mulReqLower :: Bit 1
    -- Are the operands signed or unsigned?
  , mulReqUnsignedA :: Bit 1
  , mulReqUnsignedB :: Bit 1
  } deriving (Generic, Bits)

-- | Scalar multiplier unit interface
type MulUnit t_id =
  Server (t_id, MulReq)
         (t_id, ResumeReq)

-- | Vector multiplier unit interface
type VecMulUnit n t_id =
  Server (t_id, Vec n (Option MulReq))
         (t_id, Vec n (Option ResumeReq))

-- Half throughput multiplier
-- ==========================

-- | Half throughput vector multiplier unit
makeHalfVecMulUnit :: forall n t_id.
      (KnownNat n, Bits t_id)
   => Module (VecMulUnit n t_id)
makeHalfVecMulUnit = do
  let vecSize = valueOf @n

  -- Registers for request and result
  reqRegs <- replicateM vecSize (makeReg dontCare)
  resultRegs <- replicateM vecSize (makeReg dontCare)

  -- Do we have space to store the result?
  fullReg <- makeReg false

  -- Register for request id
  idReg <- makeReg dontCare

  return
    Server {
      reqs =
        Sink {
          canPut = inv fullReg.val
        , put = \(id, reqVec) -> do
            fullReg <== true
            idReg <== id
            sequence_
              [ do let msbA = at @31 (req.val.mulReqA)
                   let msbB = at @31 (req.val.mulReqB)
                   let extA = req.val.mulReqUnsignedA ? (0, msbA)
                   let extB = req.val.mulReqUnsignedB ? (0, msbB)
                   let mulA = extA # req.val.mulReqA
                   let mulB = extB # req.val.mulReqB
                   reqReg <== req
                   resultReg <== slice @63 @0 (fullMul True mulA mulB)
              | (req, reqReg, resultReg) <-
                  zip3 (toList reqVec) reqRegs resultRegs ]
        }
    , resps =
        Source {
          peek =
            ( idReg.val
            , fromList
                [ Option req.valid
                    ResumeReq {
                      resumeReqData = req.val.mulReqLower ?
                        (lower result, upper result)
                    , resumeReqCap = none
                    }
                | (req, result) <- zip (map (.val) reqRegs)
                                       (map (.val) resultRegs) ] )
        , canPeek = fullReg.val
        , consume = fullReg <== false
        }
    }

-- | Half throughput scalar multiplier unit
makeHalfMulUnit :: Bits t_id => Module (MulUnit t_id)
makeHalfMulUnit = do
  vecMulUnit <- makeHalfVecMulUnit @1
  return
    Server {
      reqs  = mapSink (\(id, req) -> (id, fromList [Option true req]))
                      vecMulUnit.reqs
    , resps = mapSource (\(id, resp) -> (id, (head (toList resp)).val))
                        vecMulUnit.resps
    }

-- Full throughput multiplier
-- ==========================

-- | Full throughput vector multiplier unit
makeFullVecMulUnit :: forall n t_id.
      (KnownNat n, Bits t_id)
   => Module (VecMulUnit n t_id)
makeFullVecMulUnit = do
  let vecSize = valueOf @n

  -- Stall wire
  stall :: Wire (Bit 1) <- makeWire false

  -- Pipeline stage triggers
  go1 :: Reg (Bit 1) <- makeDReg false
  go2 :: Reg (Bit 1) <- makeDReg false
  go3 :: Reg (Bit 1) <- makeDReg false

  -- Request id registers for each pipeline stage
  id1 :: Reg t_id <- makeReg dontCare
  id2 :: Reg t_id <- makeReg dontCare
  id3 :: Reg t_id <- makeReg dontCare

  -- Request registers for each pipeline stage
  reqs1 :: [Reg MulReq] <- replicateM vecSize $ makeReg dontCare
  reqs2 :: [Reg MulReq] <- replicateM vecSize $ makeReg dontCare
  reqs3 :: [Reg MulReq] <- replicateM vecSize $ makeReg dontCare

  -- Active mask for requests in each pipeline stage
  active1 :: Reg (Bit n) <- makeReg dontCare
  active2 :: Reg (Bit n) <- makeReg dontCare
  active3 :: Reg (Bit n) <- makeReg dontCare

  -- Operand registers (used in stage 1)
  lowerAs :: [Reg (Bit 17)] <- replicateM vecSize $ makeReg dontCare
  upperAs :: [Reg (Bit 17)] <- replicateM vecSize $ makeReg dontCare
  lowerBs :: [Reg (Bit 17)] <- replicateM vecSize $ makeReg dontCare
  upperBs :: [Reg (Bit 17)] <- replicateM vecSize $ makeReg dontCare

  -- Partial product registers (used in stage 2)
  prods0 :: [Reg (Bit 34)] <- replicateM vecSize $ makeReg dontCare
  prods1 :: [Reg (Bit 34)] <- replicateM vecSize $ makeReg dontCare
  prods2 :: [Reg (Bit 34)] <- replicateM vecSize $ makeReg dontCare

  -- Result registers (used in stage 3)
  sums :: [Reg (Bit 64)] <- replicateM vecSize $ makeReg dontCare

  -- Result queue
  resultQueue :: Queue (t_id, Vec n (Option ResumeReq)) <- makeQueue

  -- Replay stages on stall
  always do
    when (stall.val) do
      go1 <== go1.val
      go2 <== go2.val
      go3 <== go3.val

  -- Stage 1: compute partial products
  always do
    when (go1.val .&. inv stall.val) do
      go2 <== true
      id2 <== id1.val
      active2 <== active1.val
      -- Per lane logic
      let stage1 lowerA lowerB upperA upperB prod0 prod1 prod2 req1 req2 =
            do prod0 <== fullMul True (lowerA.val) (lowerB.val)
               prod1 <== fullMul True (lowerA.val) (upperB.val) +
                         fullMul True (upperA.val) (lowerB.val)
               prod2 <== fullMul True (upperA.val) (upperB.val)
               req2 <== req1.val
      sequence_ $ getZipList $
        stage1 <$> ZipList lowerAs <*> ZipList lowerBs
               <*> ZipList upperAs <*> ZipList upperBs
               <*> ZipList prods0  <*> ZipList prods1
               <*> ZipList prods2  <*> ZipList reqs1
               <*> ZipList reqs2

  -- Stage 2: sum partial products
  always do
    when (go2.val .&. inv stall.val) do
      go3 <== true
      id3 <== id2.val
      active3 <== active2.val
      zipWithM_ (<==) reqs3 (map (.val) reqs2)
      -- Per lane logic
      let stage2 prod0 prod1 prod2 sum =
            do sum <== signExtend (prod0.val)
                     + (signExtend (prod1.val) # (0 :: Bit 16))
                     + truncate (prod2.val # (0 :: Bit 32))
      sequence_ $ getZipList $
        stage2 <$> ZipList prods0 <*> ZipList prods1
               <*> ZipList prods2 <*> ZipList sums

  -- Stage 3: select result
  always do
    when (go3.val) do
      if resultQueue.notFull
        then do
          resultQueue.enq
            ( id3.val
            , fromList
                [ Option valid
                    ResumeReq {
                      resumeReqData = req.val.mulReqLower ?
                        (lower sum.val, upper sum.val)
                    , resumeReqCap = none
                    }
                | (valid, req, sum) <-
                    zip3 (toBitList active3.val) reqs3 sums ]
            )
      else do
        stall <== true
   
  return
    Server {
      reqs =
        Sink {
          canPut = inv stall.val
        , put = \(id, reqVec) -> do
            go1 <== true
            id1 <== id
            active1 <== fromBitList (map (.valid) (toList reqVec))
            -- Per-lane insertion logic
            let insert req req1 lowerA lowerB upperA upperB =
                  do let msbA = at @31 (req.val.mulReqA)
                     let msbB = at @31 (req.val.mulReqB)
                     let extA = req.val.mulReqUnsignedA ? (0, msbA)
                     let extB = req.val.mulReqUnsignedB ? (0, msbB)
                     lowerA <== false # slice @15 @0 (req.val.mulReqA)
                     lowerB <== false # slice @15 @0 (req.val.mulReqB)
                     upperA <== extA # slice @31 @16 (req.val.mulReqA)
                     upperB <== extB # slice @31 @16 (req.val.mulReqB)
                     req1 <== req.val
            sequence_ $ getZipList $
              insert <$> ZipList (toList reqVec) <*> ZipList reqs1
                     <*> ZipList lowerAs         <*> ZipList lowerBs
                     <*> ZipList upperAs         <*> ZipList upperBs
        }
    , resps = toStream resultQueue
    }
