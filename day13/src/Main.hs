module Main where

import Control.Arrow ((&&&))
import Data.List (foldl', intercalate)
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative
import Text.Regex.Applicative.Common (decimal)

import qualified Data.Set as S

data Point = Point { _x, _y :: Int } deriving (Eq, Ord, Show)
data Axis = X | Y
data Fold = Axis := Int
type Paper = S.Set Point

data Input = Input Paper [Fold]

fold :: Fold -> Paper -> Paper
fold f = S.map (transform f)
  where transform (X := i) p@(Point x y) | x < i = p
                                         | otherwise = Point (i - (x - i)) y
        transform (Y := i) p@(Point x y) | y < i = p
                                         | otherwise = Point x (i - (y - i))

part1 :: Input -> Int
part1 (Input p (f:_)) = S.size (fold f p)

part2 :: Input -> String
part2 (Input p fs) = let maxY = last [y | Y := y <- fs]
                         maxX = last [x | X := x <- fs]
                         p' = foldl' (flip fold) p fs
                     in intercalate "\n" $ do
  y <- [0..maxY]
  pure $ do
    x <- [0..maxX]
    pure $ if S.member (Point x y) p' then '#' else '.'

prepare :: String -> Input
prepare = fromMaybe (Input mempty mempty) . (=~ input)
  where input = Input <$> dots <* sym '\n' <*> many fold
        dots = S.fromList <$> many point
        fold = (:=) <$> (string "fold along " *> axis) <* sym '=' <*> decimal <* sym '\n'
        axis = Y <$ sym 'y'
           <|> X <$ sym 'x'
        point = Point <$> decimal <* sym ',' <*> decimal <* sym '\n'

main :: IO ()
main = do
  input <- prepare <$> readFile "input.txt"
  print $ part1 input
  putStrLn $ part2 input
