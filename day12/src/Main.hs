module Main where

import Prelude hiding (head, tail, last)

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Char (isUpper)
import Data.List.NonEmpty hiding (filter, length)
import Data.Maybe (fromMaybe, maybeToList)

import Text.Regex.Applicative

import qualified Data.Set as S

data Size = Big | Small deriving (Eq, Show)
newtype Cave = Cave (NonEmpty Char) deriving (Eq, Ord, Show)
data Path = Path Cave Cave deriving Show
type Input = [Path]

start, end :: Cave
start = Cave $ 's' :| "tart"
end = Cave $ 'e' :| "nd"

size :: Cave -> Size
size (Cave (h :| _)) | isUpper h = Big
                     | otherwise = Small

dest :: Cave -> Path -> Maybe Cave
dest c (Path a b) | c == a = Just b
                  | c == b = Just a
                  | otherwise = Nothing

allRoutes :: [Path] -> [NonEmpty Cave]
allRoutes paths = filter ((== end) . last) $ (start :|) <$> go (S.singleton start) start
  where go visited curr = pure [] <> do
          p <- paths
          to <- maybeToList (dest curr p)
          let valid = case size to of
                Big -> size curr == Small
                Small -> not $ S.member to visited
          guard valid
          (to :) <$> go (S.insert to visited) to

type Regex a = RE Char a

path :: Regex Path
path = Path <$> cave <* sym '-' <*> cave
  where cave = Cave <$> label
        label = liftA2 (:|) labelChar (many labelChar)
        labelChar = psym (/= '-')

part1 :: Input -> Int
part1 = length . allRoutes

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe [] . traverse (=~ path) . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
