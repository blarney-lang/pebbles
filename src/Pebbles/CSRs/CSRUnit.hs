module Pebbles.CSRs.CSRUnit where

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Stream

-- Haskell imports
import Control.Monad (forM_)

-- Control/status registers (CSRs)
-- ===============================

-- | CSR identifier / address
type CSRId = Bit 12

-- | Representing a single CSR
data CSR =
  CSR {
    -- | CSR identifier
    csrId :: Integer
    -- | Optional read action
  , csrRead :: Maybe (Action (Bit 32))
    -- | Optional write action
  , csrWrite :: Maybe (Bit 32 -> Action ())
  }

-- | CSR unit, providing ability to read and write CSRs
data CSRUnit =
  CSRUnit {
    csrUnitRead :: CSRId -> Action (Bit 32)
  , csrUnitWrite :: CSRId -> Bit 32 -> Action ()
  }

makeCSRUnit :: [CSR] -> Module CSRUnit
makeCSRUnit csrs = do
  -- CSR id for read
  csrIdReadWire :: Wire CSRId <- makeWire dontCare

  -- CSR id for write
  csrIdWriteWire :: Wire CSRId <- makeWire dontCare

  -- CSR write value
  csrWriteWire :: Wire (Bit 32) <- makeWire dontCare

  rds <- always do
    -- Handle CSR writes
    forM_ csrs \csr ->
      case csr.csrWrite of
        Nothing -> return ()
        Just wr ->
          when csrIdWriteWire.active do
            when (fromInteger csr.csrId .==. csrIdWriteWire.val) do
              wr csrWriteWire.val

    -- Handle CSR reads
    forM csrs \csr ->
      case csr.csrRead of
        Nothing -> return []
        Just rd -> do
          let cond = csrIdReadWire.active .&.
                       (fromInteger csr.csrId .==. csrIdReadWire.val)
          x <- whenR cond rd
          return [(cond, x)]

  -- Select read value
  let readVal = select (concat rds)

  return
    CSRUnit {
      csrUnitRead = \id -> do
        csrIdReadWire <== id
        return readVal
    , csrUnitWrite = \id x -> do
        csrIdWriteWire <== id
        csrWriteWire <== x
    }
