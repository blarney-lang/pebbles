module Pebbles.Util.List where

-- Helper functions on lists

-- Blarney imports
import Blarney

-- Haskell imports
import Data.List (transpose)
import qualified Prelude as P

-- | Split a list into groups of the given size
groupsOf :: Int -> [a] -> [[a]]
groupsOf n [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

-- | Select the given half of the list
selectHalf :: Interface a => Bit 1 -> [a] -> [a]
selectHalf idx xs =
  map (! idx) $ transpose $ groupsOf (length xs `div` 2) xs

-- | Select the given quarter of the list
selectQuarter :: Interface a => Bit 2 -> [a] -> [a]
selectQuarter idx xs =
  map (! idx) $ transpose $ groupsOf (length xs `div` 4) xs

-- | Apply a function N times
iterateN :: Int -> (a -> a) -> a -> a
iterateN n f a
  | n > 0 = iterateN (n-1) f (f a)
  | otherwise = a

-- | One level of a reduction tree.
--   Apply function to each pair of elements in a list.
level :: (a -> a -> a) -> [a] -> [a]
level f [] = []
level f [x] = [x]
level f (x:y:ys) = f x y : level f ys

-- | Pipelined reduction tree, with specified pipeline depth.
--   Input list must be non-empty.
pipelinedTree1 :: Bits a => Int -> (a -> a -> a) -> [a] -> a
pipelinedTree1 n f xs
  | n >= 1 = tree 1 ds xs
  | otherwise = error "Pebbles.Util.List: pipelinedTree1 length"
  where
    depth = log2ceil (length xs)
    spacing = if n <= depth
                then fromIntegral depth / fromIntegral n
                else 1
    ds = take n [P.truncate (spacing * i) | i <- [1..]]

    tree d ns [] = error "Pebbles.Util.List: pipelinedTree1 empty"
    tree d ns [x] = iterateN (length ns) buffer x
    tree d [] xs = tree (d+1) [] (level f xs)
    tree d (n:ns) xs
      | n == d = tree (d+1) ns (map buffer ys)
      | otherwise = tree (d+1) (n:ns) ys
      where ys = level f xs
