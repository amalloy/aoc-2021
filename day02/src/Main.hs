module Main where

import Control.Arrow ((&&&))
import Data.List (foldl')

data Direction = Up | Down | Forward deriving Show
data Motion = Motion Direction Int deriving Show
data Position = Position {depth, horiz :: Int} deriving Show

type Input = [Motion]

part1 :: Input -> Int
part1 = ((*) <$> depth <*> horiz) . foldl' moveBy (Position 0 0)
  where moveBy (Position d x) (Motion m n) = case m of
          Forward -> Position d (x + n)
          Up -> v (-)
          Down -> v (+)
          where v op = Position (d `op` n) x

data Missile = Missile { x, d, aim :: Int } deriving Show

part2 :: Input -> Int
part2 = ((*) <$> d <*> x) . foldl' angle (Missile 0 0 0)
  where angle (Missile x d angle) (Motion m n) = case m of
          Forward -> Missile (x + n) (d + (n * angle)) angle
          Down -> v (+)
          Up -> v (-)
          where v op = Missile x d (angle `op` n)

prepare :: String -> Input
prepare = map (parse . words) . lines
  where parse [dir, n] = Motion d (read n)
          where d = case dir of
                      "down" -> Down
                      "up" -> Up
                      "forward" -> Forward

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
