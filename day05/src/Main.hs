module Main where

import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Text.Regex.Applicative
import Text.Regex.Applicative.Common (decimal)

data Line = Line { _from, _to :: Coord } deriving (Show, Eq, Ord)
type Grid a = M.Map Coord a
type Coord = (Int, Int)

type Regex a = RE Char a

coord :: Regex Coord
coord = (,) <$> decimal <* sym ',' <*> decimal

line :: Regex Line
line = Line <$> coord <* string " -> " <*> coord <* sym '\n'

input :: Regex Input
input = many line

type Input = [Line]

part1 :: Input -> ()
part1 = const ()

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe [] . (=~ input)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
