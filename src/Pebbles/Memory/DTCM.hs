-- Data tightly-coupled memory (DTCM)

module Pebbles.Memory.DTCM where

-- Blarney imports
import Blarney
import Blarney.Stream
import Blarney.SourceSink

-- Pebbles imports
import Pebbles.Memory.Interface
import Pebbles.Memory.Alignment

-- Haskell imports
import Data.Proxy

-- Types
-- =====

-- | DTCM options
data DTCMConfig =
  DTCMConfig {
    -- Initialisation file
    dtcmInitFile :: Maybe String
    -- Size of DTCM in words
  , dtcmLogNumWords :: Int
  }

-- Implementation
-- ==============

-- | Tightly-coupled data memory with single-cycle read latency
makeDTCM :: Bits t_id => DTCMConfig -> Module (MemUnit t_id)
makeDTCM conf = 
  -- Determine address width at type level
  liftNat (conf.dtcmLogNumWords) \(_ :: Proxy t_addrWidth) -> do

    -- Data memory (block RAM with byte enables)
    dataMem :: RAMBE t_addrWidth 4 <- makeRAMBECore (conf.dtcmInitFile)

    -- Is response ready for consuming?
    ready :: Reg (Bit 1) <- makeReg false

    -- Current request
    reqWire :: Wire (MemReq t_id) <- makeWire dontCare

    -- Original request
    reqReg :: Reg (MemReq t_id) <- makeReg dontCare

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
        -- Formulate BRAM request
        let aw = req.memReqAccessWidth
        let addr = truncateCast (upper (req.memReqAddr) :: Bit 30)
        let byteEn = genByteEnable aw (req.memReqAddr)
        let writeVal = writeAlign aw (req.memReqData)
        -- Perform BRAM request
        when (req.memReqOp .==. memLoadOp) do
          loadBE dataMem addr
        when (req.memReqOp .==. memStoreOp) do
          storeBE dataMem addr byteEn writeVal
      -- Update ready register
      ready <== full .|. (reqWire.active .&. (req.memReqOp .==. memLoadOp))

    return
      MemUnit {
        memReqs =
          Sink {
            canPut = full.inv
          , put = \req -> do
              reqWire <== req
              -- Check for unsupported ops
              let uops = [ memAtomicOp,
                         , memLocalFenceOp
                         , memGlobalFenceOp ]
              dynamicAssert (andList [req.memReqOp .!=. op | op <- uops])
                "Atomics & fences not yet supported by DTCM"
          }
      , memResps =
          Source {
            peek =
              MemResp {
                memRespId = reqReg.val.memReqId
              , memRespData = loadMux (dataMem.outBE)
                                (reqReg.val.memReqAddr.truncate)
                                (reqReg.val.memReqAccessWidth)
                                (reqReg.val.memReqIsUnsigned)
              }
          , canPeek = ready.val
          , consume = doConsume <== true
          }
      }
