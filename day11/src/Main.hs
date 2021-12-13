module Main where

import Control.Monad.Reader
import Data.Array
import Data.Char (digitToInt)

data Parameters = Parameters ()
data Octopus = Exhausted | Energized | Latent Int deriving (Eq, Show)
type Cave = Array (Int, Int) Octopus

addEnergy :: Int -> Octopus -> Reader Parameters Octopus
addEnergy a o = pure $ case o of
  Latent b | a + b >= 10 -> Energized
           | otherwise -> Latent (a + b)
  _ -> o

step :: Cave -> Reader Parameters (Cave, Int)
step c = do
  c' <- traverse (addEnergy 1) c
  completed <- complete c'
  let numFlashes = length . filter (== Exhausted) . elems $ completed
      c'' = fmap revitalize completed
  pure (c'', numFlashes)
  where revitalize Exhausted = Latent 0
        revitalize x = x

complete :: Cave -> Reader Parameters Cave
complete c = do
  let energized = [i | (i, Energized) <- assocs c]
  case energized of
    [] -> pure c
    _ -> do
      params <- ask
      let bs = bounds c
          recipients = filter (inRange bs) $ neighbors =<< energized
          collateral = [(i, addEnergy') | i <- recipients]
          addEnergy' o = runReader (addEnergy 1 o) params
          changes = [(i, const Exhausted) | i <- energized] ++ collateral
          c' = accum (\o f -> f o) c changes
      complete c'

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = do
  dx <- [0, 1, -1]
  dy <- [0, 1, -1]
  pure (x + dx, y + dy)

type Input = Cave

part1 :: Input -> Reader Parameters Int
part1 = runCave 100 0
  where runCave 0 flashes _ = pure flashes
        runCave steps flashes c = do
          (c', fs) <- step c
          runCave (steps - 1) (fs + flashes) c'

part2 :: Input -> Reader Parameters ()
part2 i = pure ()

prepare :: String -> Input
prepare s = let rows = lines s
                bounds = ((0, 0), (length rows - 1, length (head rows) - 1))
                octopi = (Latent . digitToInt) <$> concat rows
            in listArray bounds octopi

main :: IO ()
main = do
  input <- prepare <$> readFile "input.txt"
  print (runReader (part1 input) (Parameters ()))
  print (runReader (part2 input) (Parameters ()))
