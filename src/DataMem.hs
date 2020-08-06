module DataMem where

import Blarney
import Blarney.Queue
import Blarney.Stream

-- Memory request from the pipeline
data MemReq =
  MemReq {
    memReqIsStore :: Bit 1
  , memReqAddr    :: Bit 32
  , memReqByteEn  :: Bit 4
  , memReqData    :: Bit 32
  } deriving (Generic, Bits)

-- Memory response to the pipeline
type MemResp = Bit 32

-- Data tightly-coupled memory (DTCM)
-- ==================================

-- Data memory size
type DTCMAddrWidth = 14

-- Implement data memory as block RAM with byte-enables
type DTCM = RAMBE DTCMAddrWidth 4


-- Tightly-coupled data memory with single-cycle read latency
makeDTCM :: Bool -> Stream MemReq -> Module (Stream MemResp)
makeDTCM sim reqs = do
  -- Data memory block RAM
  let ext = if sim then ".hex" else ".mif"
  dataMem :: RAMBE DTCMAddrWidth 4 <- makeRAMInitBE ("data" ++ ext)

  -- Is response ready for consuming?
  ready :: Reg (Bit 1) <- makeReg false

  -- Wire pulsed when response is consumed
  doConsume :: Wire (Bit 1) <- makeWire false

  -- Consume requests
  always do
    let full = ready.val .&. doConsume.val.inv
    if full
      then dataMem.preserveOutBE
      else do
        let req = reqs.peek
        ready <== reqs.canPeek .&. req.memReqIsStore.inv
        when (reqs.canPeek) do
          reqs.consume
          let addr = lower (upper (req.memReqAddr) :: Bit 30)
          if req.memReqIsStore
            then storeBE dataMem addr (req.memReqByteEn) (req.memReqData)
            else loadBE dataMem addr

  return $
    Source {
      peek    = dataMem.outBE
    , canPeek = ready.val
    , consume = doConsume <== true
    }
