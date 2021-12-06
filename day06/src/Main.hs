{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative
import Text.Regex.Applicative.Common (decimal)

newtype Age = Age Int deriving (Num, Eq, Ord, Show)
newtype Count = Count Int deriving (Num, Show)
type School = M.Map Age Count
type Input = School
type Regex a = RE Char a

step :: School -> School
step s = M.unionWith (+) aging born
  where aging = M.fromList $ do
          prevAge <- Age <$> [1..8]
          pure (prevAge - 1, get prevAge)
        born = let num = get 0
          in M.fromList [(6, num), (8, num)]
        get k = M.findWithDefault 0 k s

part1 :: Input -> Count
part1 = sum . (!! 80) . iterate step

part2 :: Input -> ()
part2 = const ()

ages :: Regex [Age]
ages = fmap (fmap Age) $ (:) <$> decimal <*> (many (sym ',' *> decimal))

prepare :: String -> Input
prepare s = M.fromListWith (+) $ do
  age <- fromMaybe [] . (=~ (ages <* sym '\n')) $ s
  pure (age, 1)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
