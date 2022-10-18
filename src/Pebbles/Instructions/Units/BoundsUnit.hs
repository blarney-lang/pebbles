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
data BoundsReq =
  BoundsReq {
    -- Bounds opcode
    isSetBounds :: Bit 1
  , isSetBoundsExact :: Bit 1
  , isCRAM :: Bit 1
  , isCRRL :: Bit 1
    -- Instruction operands
  , cap :: CapPipe
  , len :: Bit 32
  } deriving (Generic, Interface, Bits)

-- | Bounds unit functionality
boundsUnit :: BoundsReq -> ResumeReq
boundsUnit req =
  ResumeReq {
    resumeReqData =
      select
        [ req.isSetBounds .||. req.isSetBoundsExact --> lower finalCap
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

-- | Vector Bounds unit interface
type VecBoundsUnit n t_id =
  Server (t_id, Vec n (Option BoundsReq))
         (t_id, Vec n (Option ResumeReq))

-- | Create vector of bounds units
makeVecBoundsUnit :: forall n t_id.
      (KnownNat n, Bits t_id)
   => Module (VecBoundsUnit n t_id)
makeVecBoundsUnit = do
  inputWire <- makeWire dontCare
  resultQueue <- makePipelineQueue 1
  
  always do
    when inputWire.active do
      let (info, vec) = inputWire.val
      resultQueue.enq (info, V.map (fmap boundsUnit) vec)

  return
    Server {
      reqs =
        Sink {
          canPut = resultQueue.notFull
        , put = \r -> inputWire <== r
        }
    , resps = toSource resultQueue
    }
