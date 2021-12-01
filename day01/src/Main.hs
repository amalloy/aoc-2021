{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Main where

import Control.Arrow ((&&&))

type Input = [Int]

-- >>> zip [] undefined
-- parse error on input ‘::’
-- []

-- prop> (\(xs :: [Int]) -> null xs || part1 xs >= 0)
-- +++ OK, passed 100 tests.

numIncreasingPairwise :: (Num a, Ord a) => [a] -> Int
numIncreasingPairwise xs = length . filter (< 0) $ zipWith (-) xs (tail xs)

part1 :: Input -> Int
part1 = numIncreasingPairwise

data Three a = Three a a a deriving (Foldable, Show)

threeWindows :: [a] -> [Three a]
threeWindows xs@(_:ys@(_:zs)) = zipWith3 Three xs ys zs
threeWindows _ = []

part2 :: Input -> Int
part2 = numIncreasingPairwise . map sum . threeWindows

prepare :: String -> Input
prepare = map read . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
