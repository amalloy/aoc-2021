module Main where

import Control.Arrow ((&&&))
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as S
import Text.Regex.Applicative
import Text.Regex.Applicative.Common (decimal)

type Input = S.Seq Int

median :: S.Seq a -> Maybe a
median xs = S.lookup (S.length xs `div` 2) xs

part1 :: Input -> Maybe Int
part1 input = do
  med <- median input
  pure . sum . fmap (abs . (subtract med)) $ input

cost :: Int -> Int -> Int
cost from to = let n = (abs $ from - to)
               in n * (n + 1) `div` 2

part2 :: Input -> Int
part2 crabs = case (S.viewl crabs, S.viewr crabs) of
  (lo S.:< _, _ S.:> hi) -> minimum $ do
    pos <- [lo..hi]
    pure . sum . fmap (cost pos) $ crabs
  _ -> 0

prepare :: String -> Input
prepare = S.fromList . sort . fromMaybe [] . (=~ input)
  where input = many (decimal <* anySym)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
