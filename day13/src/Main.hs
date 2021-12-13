{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Arrow ((&&&))
import Data.List (foldl', intercalate)
import Data.Maybe (fromMaybe)

import Control.Lens hiding (Fold)

import Text.Regex.Applicative
import Text.Regex.Applicative.Common (decimal)

import qualified Data.Set as S

data Point = Point { _x, _y :: Int } deriving (Eq, Ord, Show)
data Axis = X | Y
data Fold = Axis := Int
type Paper = S.Set Point
makeLenses ''Point

data Input = Input Paper [Fold]

fold :: Fold -> Paper -> Paper
fold (axis := center) = S.map (over (field axis) reflect)
  where field X = x
        field Y = y
        reflect coord = center - (abs (coord - center))

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
