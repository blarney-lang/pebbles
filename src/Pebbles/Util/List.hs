module Pebbles.Util.List where

-- Helper functions on lists

-- Blarney imports
import Blarney

-- Haskell imports
import Data.List (transpose)

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
