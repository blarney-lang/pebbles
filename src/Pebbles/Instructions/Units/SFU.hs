module Pebbles.Instructions.Units.SFU where

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.Stream
import Blarney.SourceSink
import Blarney.TaggedUnion
import Blarney.ClientServer
import Blarney.Interconnect
import Blarney.Vector (Vec, fromList, toList)
import qualified Blarney.Vector as V

-- Pebbles imports
import Pebbles.Pipeline.Interface
import Pebbles.Instructions.Units.DivUnit
import Pebbles.Instructions.Units.BoundsUnit

-- CHERI imports
import CHERI.CapLib

-- Interface
-- =========

-- SFU request
data SFUReq =
  SFUReq {
    kind :: SFUReqKind
  , opA :: Bit 32
  , opB :: Bit 32
  , capA :: CapMemMeta
  }
  deriving (Generic, Bits, Interface)

-- Request kind
type SFUReqKind =
  TaggedUnion
    [ "div" ::: SFUDivReq
    , "bounds" ::: SFUBoundsReq
    ]

-- Division request
data SFUDivReq =
  SFUDivReq {
    -- Signed or unsigned division?
    isSigned :: Bit 1
    -- Do we want the quotient or remainder?
  , getRemainder :: Bit 1
  }
  deriving (Generic, Bits, Interface)

-- Bounds request
data SFUBoundsReq =
  SFUBoundsReq {
    -- Bounds opcode 
    isGetBase :: Bit 1
  , isGetLen :: Bit 1
  , isSetBounds :: Bit 1
  , isSetBoundsExact :: Bit 1
  , isCRAM :: Bit 1
  , isCRRL :: Bit 1
  }
  deriving (Generic, Bits, Interface)

-- SFU unit interface
type SFU t_id =
  Server (t_id, SFUReq)
         (t_id, ResumeReq)

-- Vector SFU unit interface
type VecSFU n t_id =
  Server (t_id, Vec n (Option SFUReq))
         (t_id, Vec n (Option ResumeReq))

-- SFU configuration
data SFUConfig =
  SFUConfig {
    enDivUnit :: Bool
  , divLatency :: Int
  , enBoundsUnit :: Bool
  }

-- Create SFU
makeSFU :: Bits t_id => SFUConfig -> Module (SFU t_id)
makeSFU c
  | not (c.enDivUnit || c.enBoundsUnit) = return nullServer
  | otherwise = do
      divUnit <- if c.enDivUnit
        then makeFullDivUnit c.divLatency
        else return nullServer
      boundsUnit <- if c.enDivUnit
        then makeBoundsUnit
        else return nullServer

      return
        Server {
          reqs =
            Sink {
              canPut = divUnit.reqs.canPut .&&. boundsUnit.reqs.canPut
            , put = \(info, req) ->
                if req.kind `is` #div
                  then do
                    let kind = untag #div req.kind
                    let divReq = 
                          DivReq {
                            divReqNum = req.opA
                          , divReqDenom = req.opB
                          , divReqIsSigned = kind.isSigned
                          , divReqGetRemainder = kind.getRemainder
                          }
                    divUnit.reqs.put (info, divReq)
                  else do
                    let kind = untag #bounds req.kind
                    let boundsReq = 
                          BoundsReq {
                            isGetBase = kind.isGetBase
                          , isGetLen = kind.isGetLen
                          , isSetBounds = kind.isSetBounds
                          , isSetBoundsExact = kind.isSetBoundsExact
                          , isCRAM = kind.isCRAM
                          , isCRRL = kind.isCRRL
                          , cap = req.capA # req.opA
                          , len = req.opB
                          } 
                    boundsUnit.reqs.put (info, boundsReq)
            }
        , resps = divUnit.resps `mergeTwo` boundsUnit.resps
        }


-- Create (single-element) vector SFU
makeVecSFU :: forall t_id.
     Bits t_id
  => SFUConfig
  -> Module (VecSFU 1 t_id)
makeVecSFU c = do
  sfu <- makeSFU c
  
  return
    Server {
      reqs = mapSink
               ((\(id, req) -> (id, (head (toList req)).val)))
               sfu.reqs
    , resps = mapSource
                (\(id, resp) -> (id, fromList [Option true resp]))
                sfu.resps
    }
