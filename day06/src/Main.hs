{-# LANGUAGE GeneralizedNewtypeDeriving, ApplicativeDo #-}

module Main where

import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative
import Text.Regex.Applicative.Common (decimal)

newtype Age = Age Int deriving (Num, Eq, Ord, Show, Enum)
newtype Count = Count Integer deriving (Num, Show)
type School = M.Map Age Count
type Input = School
type Regex a = RE Char a

-- M = Matrix _
-- V = today's fish
-- V * M = tomorrow's fish
-- V * (M * M) = two days from now's fish
-- V * (M * M * M) = three days from now's fish
-- ...
-- V * (M ^ 256) = solution to part 2

newtype Matrix a = Matrix (ZipList (ZipList a))

mul :: Num a => Matrix a -> Matrix a -> Matrix a
mul (Matrix a) (Matrix b) =
  let cols = sequenceA b
  in Matrix $ do
    row <- a
    pure $ do
      col <- cols
      pure (sum (liftA2 (*) row col))

step :: School -> School
step s = M.unionWith (+) aging born
  where aging = M.fromList $ do
          prevAge <- [1..8]
          pure (prevAge - 1, get prevAge)
        born = let num = get 0
          in M.fromList [(6, num), (8, num)]
        get k = M.findWithDefault 0 k s

simulate :: Int -> Input -> Count
simulate numDays = sum . (!! numDays) . iterate step

part1 :: Input -> Count
part1 = simulate 80

part2 :: Input -> Count
part2 = simulate 256

extraCredit :: Input -> Count
extraCredit = simulate 1000

ages :: Regex [Age]
ages = liftA2 (:) decimal (many (sym ',' *> decimal))

prepare :: String -> Input
prepare s = M.fromListWith (+) $ do
  age <- fromMaybe [] . (=~ (ages <* sym '\n')) $ s
  pure (age, 1)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2 &&& extraCredit) . prepare
