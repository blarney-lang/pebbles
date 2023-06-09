module Pebbles.Util.SlotManager where

-- Blarney imports
import Blarney
import Blarney.Stack
import Blarney.PulseWire

data SlotManager slot =
  SlotManager {
    push1 :: slot -> Action ()
  , push2 :: slot -> Action ()
  , push3 :: slot -> Action ()
  , push4 :: slot -> Action ()
  , top1 :: slot
  , top2 :: slot
  , pop1 :: Action ()
  , pop2 :: Action ()
  , clear :: Action ()
  , notFull :: Bit 1
  , notEmpty :: Bit 1
  }
  deriving (Generic, Interface)

makeSlotManager :: (Bits slot, Interface slot) => 
     Int
     -- ^ Max number of slots
  -> Module (SlotManager slot)
makeSlotManager size = do
  let logSize = log2ceil size

  stk1 <- makeSizedStack (size-2)
  stk2 <- makeSizedStack (size-2)
  stk3 <- makeSizedStack (size-2)
  stk4 <- makeSizedStack (size-2)
  let stks = [stk1, stk2, stk3, stk4]

  stkPushNext :: Reg (Bit 2) <- makeReg 0
  stkPopNext :: Reg (Bit 2) <- makeReg 0

  stkPush1 :: Wire slot <- makeWire dontCare
  stkPush2 :: Wire slot <- makeWire dontCare
  stkPush3 :: Wire slot <- makeWire dontCare
  stkPush4 :: Wire slot <- makeWire dontCare
  let stkPushNext1 = stkPushNext.val
  let stkPushNext2 = stkPushNext1 + zeroExtend stkPush1.active
  let stkPushNext3 = stkPushNext2 + zeroExtend stkPush2.active
  let stkPushNext4 = stkPushNext3 + zeroExtend stkPush3.active

  stkPop1 <- makePulseWire
  stkPop2 <- makePulseWire
  let stkPopNext1 = stkPopNext.val
  let stkPopNext2 = stkPopNext1 + zeroExtend stkPop1.val

  clearWire <- makePulseWire

  always do
    when stkPush1.active do (stks!stkPushNext1).push stkPush1.val
    when stkPush2.active do (stks!stkPushNext2).push stkPush2.val
    when stkPush3.active do (stks!stkPushNext3).push stkPush3.val
    when stkPush4.active do (stks!stkPushNext4).push stkPush4.val
    let pushInc :: Bit 2 = zeroExtend stkPush1.active +
                           zeroExtend stkPush2.active +
                           zeroExtend stkPush3.active +
                           zeroExtend stkPush4.active
    let popInc :: Bit 2 = zeroExtend stkPop1.val +
                          zeroExtend stkPop2.val
    if clearWire.val
      then do
        stkPushNext <== 0
        stkPopNext <== 0
      else do
        stkPushNext <== stkPushNext.val + pushInc
        stkPopNext <== stkPopNext.val + popInc

  return
    SlotManager {
      push1 = \slot -> stkPush1 <== slot
    , push2 = \slot -> stkPush2 <== slot
    , push3 = \slot -> stkPush3 <== slot
    , push4 = \slot -> stkPush4 <== slot
    , top1 = (stks!stkPopNext1).top
    , top2 = (stks!stkPopNext2).top
    , pop1 = do
        stkPop1.pulse
        (stks!stkPopNext1).pop
    , pop2 = do
        stkPop2.pulse
        (stks!stkPopNext2).pop
    , clear = do
        mapM_ (.clear) stks
        clearWire.pulse
    , notFull = orList (map (.notFull) stks)
    , notEmpty = orList (map (.notEmpty) stks)
    }

nullSlotManager :: Bits slot => SlotManager slot
nullSlotManager =
  SlotManager {
    push1 = \slot -> return ()
  , push2 = \slot -> return ()
  , push3 = \slot -> return ()
  , push4 = \slot -> return ()
  , top1 = dontCare
  , top2 = dontCare
  , pop1 = return ()
  , pop2 = return ()
  , clear = return ()
  , notFull = true
  , notEmpty = false
  }
