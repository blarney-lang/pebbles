-- Interface to Altera's memory-mapped JTAG UART component

module Pebbles.IO.JTAGUART
  ( AvalonJTAGUARTIns(..)
  , AvalonJTAGUARTOuts(..)
  , makeJTAGUART
  ) where

-- Blarney imports
import Blarney
import Blarney.Stream
import Blarney.Queue

-- Avalon MM master interface
-- ==========================

-- | Inputs coming from Avalon MM slave
data AvalonJTAGUARTIns =
  AvalonDRAMIns {
    avl_jtaguart_readdata      :: Bit 32
  , avl_jtaguart_waitrequest   :: Bit 1
  } deriving (Generic, Bits, Interface)

-- | Outputs going to Avalon MM slave
data AvalonJTAGUARTOuts =
  AvalonDRAMOuts {
    avl_jtaguart_read       :: Bit 1
  , avl_jtaguart_write      :: Bit 1
  , avl_jtaguart_writedata  :: Bit 32
  , avl_jtaguart_address    :: Bit 3
  } deriving (Generic, Bits, Interface)

-- Types
-- =====

-- | Internal state of the wrapper
type State = Bit 2

-- States
s_Idle       :: State = 0  -- Do nothing
s_ReadData   :: State = 1  -- Consume char from UART, if available
s_ReadWSpace :: State = 2  -- Read UART's CSR to determine write space
s_WriteData  :: State = 3  -- Write char to UART's data register

-- Stream wrapper
-- ==============

-- | Make a streaming interface around Altera's MM JTAG UART
makeJTAGUART ::
     -- | Stream input
     Stream (Bit 8)
     -- | Avalon inputs
  -> AvalonJTAGUARTIns
     -- | Stream and Avalon outputs
  -> Module (Stream (Bit 8), AvalonJTAGUARTOuts)
makeJTAGUART streamIn avlIns = do
  -- Wrapper state
  state :: Reg (Bit 2) <- makeReg s_Idle

  -- Output queue
  outQueue :: Queue (Bit 8) <- makeShiftQueue 1

  -- This register is used to toggle between reading and writing
  tryWrite :: Reg (Bit 1) <- makeReg 0

  always do
    when (state.val .==. s_Idle) do
      tryWrite <== inv tryWrite.val
      if tryWrite.val
        then do
          when streamIn.canPeek do
            state <== s_ReadWSpace
        else do
          when outQueue.notFull do
            state <== s_ReadData

    when (state.val .==. s_ReadData) do
      dynamicAssert (outQueue.notFull) "JTAGUART: internal error 1"
      let dataIn = avlIns.avl_jtaguart_readdata
      when (inv avlIns.avl_jtaguart_waitrequest) do
        when (at @15 dataIn) do
          enq outQueue (slice @7 @0 dataIn)
        state <== s_Idle

    when (state.val .==. s_ReadWSpace) do
      let dataIn = avlIns.avl_jtaguart_readdata
      when (inv avlIns.avl_jtaguart_waitrequest) do
        state <== slice @31 @16 dataIn .>. 0 ? (s_WriteData, s_Idle)

    when (state.val .==. s_WriteData) do
      dynamicAssert (streamIn.canPeek) "JTAGUART: internal error 2"
      when (inv avlIns.avl_jtaguart_waitrequest) do
        streamIn.consume
        state <== s_Idle

  -- Avalon outputs
  let avlOuts =
        AvalonDRAMOuts {
          avl_jtaguart_read =
            (state.val .==. s_ReadData) .|.
              (state.val .==. s_ReadWSpace)
        , avl_jtaguart_write =
            state.val .==. s_WriteData
        , avl_jtaguart_writedata = zeroExtend (streamIn.peek)
        , avl_jtaguart_address =
            (state.val .==. s_ReadData) .|.
              (state.val .==. s_WriteData) ? (0, 4)
        }

  return (toStream outQueue, avlOuts)
