module Pebbles.Util.SearchQueue where

import Blarney

-- | Interface
data SearchQueue a =
  SearchQueue {
    -- | Space to insert new element?
    canInsert :: Bit 1
    -- | Add new element to end of queue
  , insert :: a -> Action ()
    -- | Remove element from front of queue
  , delete :: Action ()
    -- | Memebership test
  , member :: a -> Bit 1
  }

-- | Create searchable queue of the given size
makeSearchQueue :: (Bits a, Cmp a) => Bool -> Int -> Module (SearchQueue a)
makeSearchQueue forward size = do
  -- Queue elements
  elems <- replicateM size (makeReg dontCare)

  -- Which elements are valid
  valids <- replicateM size (makeReg false)

  -- Interface wires
  insertWire <- makeWire dontCare
  deleteWire <- makeWire false

  always do
    if insertWire.active
      then do
        -- Overwrite first element and shift rest by one place
        zipWithM (<==) elems (insertWire.val : map val elems)
        -- Increase number of valid elements, unless deleting
        when (inv deleteWire.val) do
          zipWithM_ (<==) valids (true : map val valids)
      else do
        when (deleteWire.val) do
          zipWithM_ (<==) valids (drop 1 (map val valids) ++ [false]) 

  return
    SearchQueue {
      canInsert = inv (last valids).val
    , insert = (insertWire <==)
    , delete = deleteWire <== true
    , member = \a ->
        orList [ valid.val .&&. a .==. elem.val
               | (valid, elem) <- zip valids elems ]
          .||. (if forward
                  then insertWire.active .&&. insertWire.val .==. a
                  else false)
    }
