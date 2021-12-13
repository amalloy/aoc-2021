module Main where

import Control.Arrow ((&&&))
import Data.Array
import Data.Char (digitToInt)

data Octopus = Exhausted | Energized | Latent Int deriving (Eq, Show)
type Cave = Array (Int, Int) Octopus

addEnergy :: Int -> Octopus -> Octopus
addEnergy a o = case o of
  Latent b | a + b >= 10 -> Energized
           | otherwise -> Latent (a + b)
  _ -> o

step :: Cave -> (Cave, Int)
step c = let c' = fmap (addEnergy 1) c
             completed = complete c'
             numFlashes = length . filter (== Exhausted) . elems $ completed
             c'' = fmap revitalize completed
         in (c'', numFlashes)
  where revitalize Exhausted = Latent 0
        revitalize x = x

complete :: Cave -> Cave
complete c =
  case [i | (i, Energized) <- assocs c] of
    [] -> c
    energized ->
      let bs = bounds c
          recipients = filter (inRange bs) $ neighbors =<< energized
          collateral = [(i, addEnergy 1) | i <- recipients]
          changes = [(i, const Exhausted) | i <- energized] ++ collateral
          c' = accum (\o f -> f o) c changes
      in complete c'

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = do
  dx <- [0, 1, -1]
  dy <- [0, 1, -1]
  pure (x + dx, y + dy)

type Input = Cave

part1 :: Input -> Int
part1 = runCave 100 0
  where runCave 0 flashes _ = flashes
        runCave steps flashes c = runCave (steps - 1) (fs + flashes) c'
          where (c', fs) = step c

part2 :: Input -> Int
part2 cave = go cave
  where n = rangeSize $ bounds cave
        go c = case step c of
          (c', f) | f == n -> 1
                  | otherwise -> succ $ go c'

prepare :: String -> Input
prepare s = let rows = lines s
                bounds = ((0, 0), (length rows - 1, length (head rows) - 1))
                octopi = (Latent . digitToInt) <$> concat rows
            in listArray bounds octopi

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
