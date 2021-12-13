module Main where

import Prelude hiding (head, tail, last)

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Char (isUpper)
import Data.List.NonEmpty hiding (filter, length, map)
import Data.Maybe (fromMaybe, maybeToList)

import Text.Regex.Applicative

import qualified Data.Set as S

data Size = Big | Small deriving (Eq, Show)
newtype Cave = Cave (NonEmpty Char) deriving (Eq, Ord, Show)
data Path = Path Cave Cave deriving Show
data CheatMode = CheatingAllowed | NoCheating deriving Eq
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

allRoutes :: CheatMode -> [Path] -> [NonEmpty Cave]
allRoutes cm paths = filter ((== end) . last) . map (start :|) $ go S.empty start cm
  where go visited curr cheatMode = pure [] <> do
          guard $ curr /= end
          p <- paths
          to <- maybeToList (dest curr p)
          guard $ to /= start
          let (valid, cheatMode') = case (size to, S.member to visited, cheatMode) of
                (Big, _, cm) -> (size curr == Small, cm)
                (Small, False, cm) -> (True, cm)
                (Small, True, cm) -> (cm == CheatingAllowed, NoCheating)
          guard valid
          (to :) <$> go (S.insert to visited) to cheatMode'

type Regex a = RE Char a

path :: Regex Path
path = Path <$> cave <* sym '-' <*> cave
  where cave = Cave <$> label
        label = liftA2 (:|) labelChar (many labelChar)
        labelChar = psym (/= '-')

part1 :: Input -> Int
part1 = length . allRoutes NoCheating

part2 :: Input -> Int
part2 = length . allRoutes CheatingAllowed

prepare :: String -> Input
prepare = fromMaybe [] . traverse (=~ path) . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
