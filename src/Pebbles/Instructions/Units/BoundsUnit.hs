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
boundsUnit :: BoundsReq Cap -> ResumeReq
boundsUnit req =
  ResumeReq {
    resumeReqData =
      select
        [ req.isGetBase --> req.cap.capBase
        , req.isGetLen -->
            if at @32 req.cap.capLength then ones else lower req.cap.capLength
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
   addr = getAddr req.cap.capPipe
   sbres = setBoundsCombined req.cap.capPipe req.len
   invalidate =
     orList
       [ inv (isValidCap req.cap.capPipe)
       , isSealed req.cap.capPipe
       , addr .<. req.cap.capBase
       , zeroExtend addr + zeroExtend req.len .>. req.cap.capTop
       , req.isSetBoundsExact .&&. inv sbres.exact
       ]
   validCap = isValidCap req.cap.capPipe .&&. inv invalidate
   (finalCapTag, finalCap) = toMem (setValidCap sbres.cap validCap)

toCap :: BoundsReq CapMem -> BoundsReq Cap
toCap (BoundsReq {..}) = BoundsReq {cap = decodeCapMem cap, ..}

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
            inputQueue.enq (info, V.map (fmap toCap) vec)
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
