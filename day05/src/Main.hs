{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Control.Arrow ((&&&))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Text.Regex.Applicative
import Text.Regex.Applicative.Common (decimal)

data Line = Line { _from, _to :: Coord Int } deriving Show
data Coord a = Coord { _x, _y :: a }
  deriving (Show, Eq, Ord, Functor, Foldable)
instance Applicative Coord where
  pure x = Coord x x
  Coord fx fy <*> Coord x y = Coord (fx x) (fy y)

type Regex a = RE Char a

coord :: Regex (Coord Int)
coord = Coord <$> decimal <* sym ',' <*> decimal

line :: Regex Line
line = Line <$> coord <* string " -> " <*> coord <* sym '\n'

input :: Regex Input
input = many line

type Input = [Line]

diagonal :: Line -> Bool
diagonal (Line from to) = not . any (== 0) $ abs <$> liftA2 (-) to from

pointsOnLine :: Line -> [Coord Int]
pointsOnLine (Line from to) =
  let vector = liftA2 (-) to from
      len = 1 + maximum (abs <$> vector)
      unit = signum <$> vector
      move = liftA2 (+) unit
  in take len $ iterate move from

importLines :: [Line] -> M.Map (Coord Int) Int
importLines = foldl' add M.empty . (>>= pointsOnLine)
  where add m k = M.insertWith (+) k 1 m

countOverlaps :: M.Map k Int -> Int
countOverlaps = length . filter (> 1) . M.elems

part1 :: Input -> Int
part1 = countOverlaps . importLines . filter (not . diagonal)

part2 :: Input -> Int
part2 = countOverlaps . importLines

prepare :: String -> Input
prepare = fromMaybe [] . (=~ input)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
