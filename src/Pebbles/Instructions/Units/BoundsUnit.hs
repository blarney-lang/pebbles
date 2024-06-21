{-# LANGUAGE RecordWildCards #-}
module Pebbles.Instructions.Units.BoundsUnit where

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.Stream
import Blarney.SourceSink
import Blarney.ClientServer
import Blarney.Vector (Vec, fromList, toList)
import qualified Blarney.Vector as V

-- Pebbles imports
import Pebbles.Pipeline.Interface

-- CHERI imports
import CHERI.CapLib

-- Interface
-- =========

-- | Bounds unit request
data BoundsReq cap =
  BoundsReq {
    -- Bounds opcode
    isGetBase :: Bit 1
  , isGetLen :: Bit 1
  , isSetBounds :: Bit 1
  , isSetBoundsExact :: Bit 1
  , isCRAM :: Bit 1
  , isCRRL :: Bit 1
    -- Instruction operands
  , cap :: cap
  , len :: Bit 32
  } deriving (Generic, Interface, Bits)

-- | Bounds unit functionality
boundsUnit :: BoundsReq CapPipe -> ResumeReq
boundsUnit req =
  ResumeReq {
    resumeReqData =
      select
        [ req.isGetBase --> boundsInfo.base
        , req.isGetLen -->
            if at @32 boundsInfo.length then ones else lower boundsInfo.length
        , req.isSetBounds .||. req.isSetBoundsExact --> lower finalCap
        , req.isCRAM --> sbres.mask
        , req.isCRRL --> sbres.length ]
    , resumeReqDataTagBit =
        if req.isSetBounds .||. req.isSetBoundsExact
          then validCap else false
    , resumeReqCap =
        Option (req.isSetBounds .||. req.isSetBoundsExact)
               (finalCapTag # upper finalCap)
    }
  where
   addr = getAddr req.cap
   sbres = setBoundsCombined req.cap req.len
   boundsInfo = getBoundsInfo req.cap
   invalidate =
     orList
       [ inv (isValidCap req.cap)
       , isSealed req.cap
       , addr .<. boundsInfo.base
       , zeroExtend addr + zeroExtend req.len .>. boundsInfo.top
       , req.isSetBoundsExact .&&. inv sbres.exact
       ]
   validCap = isValidCap req.cap .&&. inv invalidate
   (finalCapTag, finalCap) = toMem (setValidCap sbres.cap validCap)

toCapPipe :: BoundsReq CapMem -> BoundsReq CapPipe
toCapPipe (BoundsReq {..}) = BoundsReq {cap = fromMem (unpack cap), ..}

-- | Bounds unit interface
type BoundsUnit t_id =
  Server (t_id, BoundsReq CapMem)
         (t_id, ResumeReq)

-- | Vector Bounds unit interface
type VecBoundsUnit n t_id =
  Server (t_id, Vec n (Option (BoundsReq CapMem)))
         (t_id, Vec n (Option ResumeReq))

-- | Create vector of bounds units
makeVecBoundsUnit :: forall n t_id.
      (KnownNat n, Bits t_id)
   => Module (VecBoundsUnit n t_id)
makeVecBoundsUnit = do
  inputQueue <- makePipelineQueue 1
  resultQueue <- makePipelineQueue 1
  
  always do
    when (inputQueue.canDeq .&&. resultQueue.notFull) do
      let (info, vec) = inputQueue.first
      inputQueue.deq
      resultQueue.enq (info, V.map (fmap boundsUnit) vec)

  return
    Server {
      reqs =
        Sink {
          canPut = inputQueue.notFull
        , put = \(info, vec) ->
            inputQueue.enq (info, V.map (fmap toCapPipe) vec)
        }
    , resps = toSource resultQueue
    }

-- | Single bounds unit
makeBoundsUnit :: Bits t_id => Module (BoundsUnit t_id)
makeBoundsUnit = do
  vecBoundsUnit <- makeVecBoundsUnit @1
  return
    Server {
      reqs  = mapSink (\(id, req) -> (id, fromList [Option true req]))
                      vecBoundsUnit.reqs
    , resps = mapSource (\(id, resp) -> (id, (head (toList resp)).val))
                        vecBoundsUnit.resps
    }
