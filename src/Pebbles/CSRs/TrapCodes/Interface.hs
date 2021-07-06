module Pebbles.CSRs.TrapCodes.Interface where

-- Blarney imports
import Blarney

data TrapCode =
  TrapCode {
    trapCodeIsInterrupt :: Bit 1
    -- ^ Interrupt or exception?
  , trapCodeCause :: Bit 31
    -- ^ Trap cause
  , trapCodeCapCause :: Bit 5
    -- ^ Capability exception cause
  } deriving (Generic, Interface, Bits, FShow)

excCode :: Bit 31 -> TrapCode
excCode c =
  TrapCode {
    trapCodeIsInterrupt = 0
  , trapCodeCause = c
  , trapCodeCapCause = 0
  }

intCode :: Bit 31 -> TrapCode
intCode c =
  TrapCode {
    trapCodeIsInterrupt = 1
  , trapCodeCause = c
  , trapCodeCapCause = 0
  }

excCapCode :: Bit 5 -> TrapCode
excCapCode c =
  TrapCode {
    trapCodeIsInterrupt = 0
  , trapCodeCause = 32
  , trapCodeCapCause = c
  }
