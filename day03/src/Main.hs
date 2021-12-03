{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (transpose, foldl')
import Control.Arrow ((&&&))

data Bit = Zero | One deriving (Eq, Ord, Show, Enum)

data Freq = Freq { ones, zeroes :: Int } deriving Show
instance Semigroup Freq where
  Freq o z <> Freq p x = Freq (o + p) (z + x)
instance Monoid Freq where
  mempty = Freq 0 0

asNumber :: [Bit] -> Int
asNumber = foldl' go 0
  where go acc b = 2 * acc + fromEnum b

bitCounts :: [Bit] -> Freq
bitCounts = foldMap toFreq
  where toFreq Zero = Freq 0 1
        toFreq One = Freq 1 0

type Input = [[Bit]]

part1 :: Input -> Int
part1 nums = (*) <$> epsilon <*> gamma $ map bitCounts (transpose nums)
  where epsilon = mkNumber isEpsilon
        gamma = mkNumber (not . isEpsilon)
        isEpsilon freq = ones freq > zeroes freq
        mkNumber sel freqs = asNumber (map (mkBit . sel) freqs)
        mkBit True = One
        mkBit False = Zero

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = map (map toBit) . lines
  where toBit '0' = Zero
        toBit '1' = One

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
