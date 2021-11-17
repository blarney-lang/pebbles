-- Functions for capability serialisation/deserialisation

module Pebbles.Memory.CapSerDes where
  
-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream
import Blarney.Option
import Blarney.SourceSink
import Blarney.Vector (Vec, toList, fromList)

-- Local imports
import Pebbles.Memory.Interface
import Pebbles.Pipeline.Interface

-- Serialisation
-- =============

-- | Convert 'CapMemReq's to 'MemReq's by serialisation
makeCapMemReqSerialiser :: forall n t_id. (KnownNat n, Bits t_id) =>
     Sink (t_id, Vec n (Option MemReq))
     -- ^ Sink for standard memory requests
  -> Module (Sink (t_id, Vec n (Option CapMemReq)))
     -- ^ Sink for capability memory requests
makeCapMemReqSerialiser memReqSink = do
  -- Are we currently serialising a request?
  busy :: Reg (Bit 1) <- makeReg false

  -- Register for request currently being serialised
  reqReg :: Reg (t_id, Vec n (Option CapMemReq)) <- makeReg dontCare

  always do
    when (busy.val .&&. memReqSink.canPut) do
      -- Put the second (final) flits of the serialised requests
      let stdReqs =
            [ Option valid
                req.capMemReqStd {
                  memReqData = req.capMemReqUpperData
                , memReqAddr = req.capMemReqStd.memReqAddr + 4
                , memReqIsFinal = true
                }
            | Option valid req <- toList reqReg.val.snd ]
      memReqSink.put (reqReg.val.fst, fromList stdReqs)
      -- Serialisation complete
      busy <== false

  return
    Sink {
      canPut = inv busy.val .&&. memReqSink.canPut
    , put = \(id, reqs) -> do
        reqReg <== (id, reqs)
        -- A capability access will require serialisation
        busy <== orList
          [ valid .&&. r.capMemReqIsCapAccess
          | Option valid r <- toList reqs ]
        -- Put the first flits of the serialised requests 
        let stdReqs =
              [ Option valid
                  r.capMemReqStd {
                    -- If it's a capability access, disable final bit
                    memReqIsFinal = inv r.capMemReqIsCapAccess
                  }
              | Option valid r <- toList reqs ]
        memReqSink.put (id, fromList stdReqs)
    }

-- Deserialisation
-- ===============

-- | Convert memory response flit(s) to pipeline resume request
-- TODO: drop this, and expose transactions to pipeline;
--       move isFinal bit into first component of stream pair
makeMemRespDeserialiser :: forall n t_id. (KnownNat n, Bits t_id) =>
     Bool
     -- ^ Is CHERI enabled?
  -> Stream (t_id, Vec n (Option MemResp))
     -- ^ Stream of memory response flits
  -> Module (Stream (t_id, Vec n (Option ResumeReq)))
     -- ^ Stream of resume requests
makeMemRespDeserialiser enableCHERI memResps
  | not enableCHERI =
      return
        memResps {
          peek = ( memResps.peek.fst
                 , fromList
                     [ Option valid
                         ResumeReq {
                           resumeReqData = r.memRespData
                         , resumeReqCap = none
                         }
                     | Option valid r <- toList memResps.peek.snd ]
                 )
        }
  | otherwise = do
      let vecSize = valueOf @n

      -- Output buffer
      outQueue :: Queue (t_id, Vec n (Option ResumeReq)) <- makePipelineQueue 1

      -- Count flits in response
      flitCount :: Reg (Bit 1) <- makeReg 0

      -- Tag bit accumulator
      tagBitReg :: Reg (Bit n) <- makeReg ones

      -- Data acummulator
      dataRegs :: [Reg (Option (Bit 32))] <-
        replicateM vecSize (makeReg dontCare)

      always do
        when (memResps.canPeek) do
          let (respId, respVec) = memResps.peek

          -- Is this the final flit of a transaction?
          let isFinal = orList
                [ valid .&&. r.memRespIsFinal
                | Option valid r <- toList respVec ]

          -- Currently, only one or two response flits are expected
          -- Two-flit responses are assumed to be capabilities
          dynamicAssert (flitCount.val .!=. 0 .==>. isFinal)
            "makeMemRespToResumeReq: too many flits in memory response"

          -- The capability is valid if all its tag bits are set
          let isValid = tagBitReg.val .&.
                fromBitList [ valid .&&. r.memRespDataTagBit
                            | Option valid r <- toList respVec ]

          if isFinal
            then do
              when outQueue.notFull do
                -- Reset accumlators for next transaction
                flitCount <== 0
                tagBitReg <== ones
                -- Create resume request
                let vec = fromList
                      [ Option valid
                          ResumeReq {
                           resumeReqData =
                             if flitCount.val .==. 1
                               then reg.val.val else resp.memRespData
                         , resumeReqCap =
                             Option (flitCount.val .==. 1)
                                    (tag # resp.memRespData)
                         }
                      | (Option valid resp, reg, tag) <-
                          zip3 (toList respVec) dataRegs (toBitList isValid) ]
                enq outQueue (respId, vec)
                -- Consume flit
                memResps.consume
                -- Sanity check: valid bit of 1st and 2nd flits are consistent
                let consistent = andList
                      [ resp.valid .==. reg.val.valid
                      | (resp, reg) <- zip (toList respVec) dataRegs ]
                dynamicAssert consistent
                  "makeMemRespDeserialiser: inconsistent responses"
            else do
              -- Accumulate response
              tagBitReg <== isValid
              zipWithM_ (<==) dataRegs (map (fmap (.memRespData)) $
                                         toList respVec)
              flitCount <== flitCount.val + 1
              -- Consume flit
              memResps.consume

      return (toStream outQueue)
