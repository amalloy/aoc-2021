module Main where

import Control.Arrow ((&&&))
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Min(..), Max(..))
import Text.Regex.Applicative
import Text.Regex.Applicative.Common (decimal)

data Input = Input {_min, _max :: Int, _crabs :: NonEmpty Int}

solve :: (Int -> Int) -> Input -> Int
solve cost (Input min max crabs) = minimum $ do
    pos <- [min..max]
    pure . sum . fmap (cost . abs . (pos -)) $ crabs

part1 :: Input -> Int
part1 = solve abs

part2 :: Input -> Int
part2 = solve cost
  where cost n = n * (n + 1) `div` 2

prepare :: String -> Maybe Input
prepare = mkCrabs . fromMaybe [] . (=~ input)
  where mkCrabs input = let (min, max) = coerce $ foldMap (Just . Min &&& Just . Max) input
                        in liftA3 Input min max (nonEmpty input)
        input = many (decimal <* anySym)

main :: IO ()
main = readFile "input.txt" >>= print . maybe (0, 0) (part1 &&& part2) . prepare
