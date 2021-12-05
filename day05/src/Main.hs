module Main where

import Control.Arrow ((&&&))
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Text.Regex.Applicative
import Text.Regex.Applicative.Common (decimal)

data Line = Line { _from, _to :: Coord } deriving (Show, Eq, Ord)
type Grid a = M.Map Coord a
type Coord = (Int, Int)
data Orientation = Vertical | Horizontal | Diagonal deriving (Show, Enum, Eq, Ord)

type Regex a = RE Char a

coord :: Regex Coord
coord = (,) <$> decimal <* sym ',' <*> decimal

line :: Regex Line
line = Line <$> coord <* string " -> " <*> coord <* sym '\n'

input :: Regex Input
input = many line

type Input = [Line]

orientation :: Line -> Orientation
orientation (Line (x1, y1) (x2, y2)) | x1 == x2 = Vertical
                                     | y1 == y2 = Horizontal
                                     | otherwise = Diagonal

importOrthoganalLines :: [Line] -> Grid [Line]
importOrthoganalLines = foldl' addToGrid M.empty
  where addToGrid m line = case orientation line of
          Diagonal -> m
          Vertical -> let Line (x, y1) (_, y2) = line
                          from = min y1 y2
                          to = max y1 y2
                          addPoint m y = M.insertWith (<>) (x, y) [line] m
                      in foldl' addPoint m [from..to]
          Horizontal -> let Line (x1, y) (x2, _) = line
                            from = min x1 x2
                            to = max x1 x2
                            addPoint m x = M.insertWith (<>) (x, y) [line] m
                        in foldl' addPoint m [from..to]

countOverlaps :: Foldable f => f [a] -> Int
countOverlaps = length . filter ((> 1) . length) . toList

part1 :: Input -> Int
part1 = countOverlaps . importOrthoganalLines

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe [] . (=~ input)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
