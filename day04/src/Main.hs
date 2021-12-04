{-# LANGUAGE ApplicativeDo, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Main where

import Text.Regex.Applicative ( sym, Alternative(many), RE, (=~), psym )

import Control.Arrow ((&&&))
import Text.Regex.Applicative.Common (decimal)
import Data.Maybe (fromJust, isNothing, catMaybes)
import Data.Foldable (toList)
import Data.Char (isSpace)

data Five a = Five a a a a a deriving (Foldable, Functor, Traversable, Show)
instance Applicative Five where
  pure x = Five x x x x x
  Five fa fb fc fd fe <*> Five a b c d e = Five (fa a) (fb b) (fc c) (fd d) (fe e)
newtype Board a = Board (Five (Five a)) deriving (Show, Functor, Foldable)

data Input = Input [Int] [Board Int] deriving Show
data State = State {_lastCalled :: Int, _boards :: [Board (Maybe Int)]} deriving Show

type Regex a = RE Char a

sepBy :: Alternative f => f a -> f b -> f [a]
p `sepBy` delim = (:) <$> p <*> many (delim *> p)

numbers :: Regex [Int]
numbers = decimal `sepBy` sym ','

fiveTimes :: Applicative f => f a -> f (Five a)
fiveTimes p = Five <$> p <*> p <*> p <*> p <*> p

board :: Regex (Board Int)
board = Board <$> fiveTimes (sym '\n' *> row)
  where row = fiveTimes $ many (sym ' ') *> decimal

input :: Regex Input
input = Input <$> numbers <*> many (sym '\n' *> board) <* many (psym isSpace)

mark :: Eq a => a -> Board (Maybe a) -> Board (Maybe a)
mark x = fmap (erase (Just x))
  where erase x y | x == y = Nothing
                  | otherwise = y

winner :: Board (Maybe a) -> Bool
winner (Board b) = any (all isNothing) b || any (all isNothing) (sequenceA b)

states :: Input -> [State]
states (Input nums boards) = go nums (fmap (fmap Just) boards)
  where go [] _ = []
        go (n:ns) boards = State n boards' : go ns (filter (not . winner) boards')
          where boards' = mark n <$> boards

score :: Num a => a -> Board (Maybe a) -> a
score n b = n * (sum . catMaybes . toList $ b)

part1 :: Input -> Int
part1 = go . states
  where go [] = error "no winner"
        go (State n bs : ss) = case filter winner bs of
          [] -> go ss
          b : _ -> score n b

part2 :: Input -> Int
part2 = go . states
  where go (State n [b] : _) | winner b = score n b
        go (_ : ss) = go ss
        go [] = error "no winner"

prepare :: String -> Input
prepare = fromJust . (=~ input)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
