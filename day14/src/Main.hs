module Main where

import Control.Arrow ((&&&))
import Data.Char (isUpper)
import Data.Maybe (fromMaybe)

import qualified Data.Map.Strict as M
import Text.Regex.Applicative

newtype Element = Element Char deriving (Eq, Ord, Show)
data Pair a = Pair a a deriving (Eq, Ord, Show)
data Rule a = Rule (Pair a) a deriving (Show)

type Polymer a = M.Map (Pair a) Int
data Input = Input (Polymer Element) [Rule Element] deriving Show

part1 :: Input -> ()
part1 = const ()

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe (Input mempty mempty) . (=~ input)
  where input = Input <$> polymer <* string "\n\n" <*> many rule
        element = Element <$> psym isUpper
        rule = Rule <$> pair <* string " -> " <*> element <* sym '\n'
        pair = Pair <$> element <*> element
        polymer = mkPolymer <$> many element
          where mkPolymer es = M.fromListWith (+) $ zipWith entry es (tail es)
                entry a b = (Pair a b, 1)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
