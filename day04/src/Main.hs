{-# LANGUAGE ApplicativeDo, DeriveFunctor, DeriveFoldable #-}

module Main where

import Text.Regex.Applicative

import Control.Arrow ((&&&))
import Text.Regex.Applicative.Common (decimal)

data Five a = Five a a a a a deriving (Foldable, Functor, Show)
instance Applicative Five where
  pure x = Five x x x x x
  Five fa fb fc fd fe <*> Five a b c d e = Five (fa a) (fb b) (fc c) (fd d) (fe e)
instance Traversable Five where
  sequenceA (Five fa fb fc fd fe) = Five <$> fa <*> fb <*> fc <*> fd <*> fe
newtype Board a = Board (Five (Five a)) deriving (Show, Functor)

data Input = Input [Int] [Board Int] deriving Show

type Regex a = RE Char a

sepBy :: Regex a -> Regex b -> Regex [a]
p `sepBy` delim = (:) <$> p <*> many (delim *> p)

numbers :: Regex [Int]
numbers = decimal `sepBy` sym ','

fiveSepBy :: Regex a -> Regex b -> Regex (Five a)
fiveSepBy p delim = Five <$> p <*> dp <*> dp <*> dp <*> dp
  where dp = delim *> p

board :: Regex (Board Int)
board = Board <$> (sym '\n' *> fiveSepBy row (sym '\n'))
  where row = fiveSepBy decimal (many (sym ' '))

testInput :: String 
testInput = let b = concat . replicate 5 $ "\n1    2 4 10 20"
            in "1,2,10" ++ "\n" ++ b ++ "\n" ++ b

input :: Regex Input
input = Input <$> numbers <*> many (sym '\n' *> board)

part1 :: Input -> ()
part1 = const ()

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = undefined

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
