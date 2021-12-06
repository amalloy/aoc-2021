module Main where

import Control.Arrow ((&&&))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Text.Regex.Applicative
import Text.Regex.Applicative.Common (decimal)

data Line = Line { _from, _to :: Coord } deriving (Show, Eq, Ord)
type Coord = (Int, Int)

type Regex a = RE Char a

coord :: Regex Coord
coord = (,) <$> decimal <* sym ',' <*> decimal

line :: Regex Line
line = Line <$> coord <* string " -> " <*> coord <* sym '\n'

input :: Regex Input
input = many line

type Input = [Line]

diagonal :: Line -> Bool
diagonal (Line (x1, y1) (x2, y2)) = abs (x1 - x2) == abs (y1 - y2)

pointsOnLine :: Line -> [Coord]
pointsOnLine (Line from@(x1, y1) (x2, y2)) =
  let len = 1 + max (abs (x1 - x2)) (abs (y1 - y2))
      (dx, dy) = (signum (x2 - x1), signum (y2 - y1))
      move (x, y) = (x + dx, y + dy)
  in take len $ iterate move from

importLines :: [Line] -> M.Map Coord Int
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
