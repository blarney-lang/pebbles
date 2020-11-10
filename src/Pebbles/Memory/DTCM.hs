module Pebbles.Memory.DTCM where

-- Blarney imports
import Blarney
import Blarney.Stream
import Blarney.SourceSink

-- Pebbles imports
import Pebbles.Memory.Interface

-- Haskell imports
import Data.Proxy

-- Data tightly-coupled memory (DTCM)
-- ==================================

-- One possible implementation of MemUnit, using a block RAM

-- | DTCM options
data DTCMConfig =
  DTCMConfig {
    -- Initialisation file
    dtcmInitFile :: Maybe String
    -- Size of DTCM in words
  , dtcmLogNumWords :: Int
  }

-- | Tightly-coupled data memory with single-cycle read latency
makeDTCM :: DTCMConfig -> Module MemUnit
makeDTCM conf = 
  -- Determine address width at type level
  liftNat (conf.dtcmLogNumWords) \(_ :: Proxy t_addrWidth) -> do

    -- Data memory (block RAM with byte enables)
    dataMem :: RAMBE t_addrWidth 4 <- makeRAMBECore (conf.dtcmInitFile)

    -- Is response ready for consuming?
    ready :: Reg (Bit 1) <- makeReg false

    -- Current request
    reqWire :: Wire MemReq <- makeWire dontCare

    -- Ooriginal request
    reqReg :: Reg MemReq <- makeReg dontCare

    -- Wire pulsed when response is consumed
    doConsume :: Wire (Bit 1) <- makeWire false

    -- Can we consume another request?
    let full = ready.val .&. doConsume.val.inv

    always do
      -- Hold output of block RAM when full
      when full do
        dataMem.preserveOutBE
      -- Handle requests
      let req = reqWire.val
      when (reqWire.active) do
        reqReg <== req
        let addr = truncateCast (upper (req.memReqAddr) :: Bit 30)
        if req.memReqIsStore
          then storeBE dataMem addr (req.memReqByteEn) (req.memReqData)
          else loadBE dataMem addr
      -- Update ready register
      ready <== full .|. (reqWire.active .&. req.memReqIsStore.inv)

    return
      MemUnit {
        memReqs =
          Sink {
            canPut = full.inv
          , put = (reqWire <==)
          }
      , memResps =
          Source {
            peek =
              MemResp {
                memRespData = dataMem.outBE
              , memRespInfo = reqReg.val
              }
          , canPeek = ready.val
          , consume = doConsume <== true
          }
      }
