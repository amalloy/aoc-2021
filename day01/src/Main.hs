{-# LANGUAGE DeriveFoldable #-}

module Main where

import Control.Arrow ((&&&))
import Data.Array

type Input = Array Int Int

numIncreasingDiffsOfSize :: Int -> Array Int Int -> Int
numIncreasingDiffsOfSize delta a = length [() | i <- indices, get (i + delta) > get i]
  where get = (a !)
        indices = [lo..hi - delta]
          where (lo, hi) = bounds a

part1 :: Input -> Int
part1 = numIncreasingDiffsOfSize 1

part2 :: Input -> Int
part2 = numIncreasingDiffsOfSize 3

prepare :: String -> Input
prepare = toArray . map read . lines
  where toArray xs = listArray (0, length xs - 1) xs

main :: IO ()
main = interact $ show . (part1 &&& part2) . prepare
