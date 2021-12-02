module Main where

import Control.Arrow ((&&&))
import Data.List (foldl')

data Direction = Up | Down | Forward deriving Show
data Motion = Motion Direction Int deriving Show
data Position = Position {depth, horiz :: Int} deriving Show

type Input = [Motion]

moveBy :: Position -> Motion -> Position
moveBy (Position d x) (Motion m n) = case m of
    Forward -> Position d (x + n)
    Up -> v (-)
    Down -> v (+)
    where v op = Position (d `op` n) x


part1 :: Input -> Int
part1 = ((*) <$> depth <*> horiz) . foldl' moveBy (Position 0 0)

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = map (parse . words) . lines
  where parse [dir, n] = Motion d (read n)
          where d = case dir of
                      "down" -> Down
                      "up" -> Up
                      "forward" -> Forward

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
