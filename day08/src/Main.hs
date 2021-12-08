module Main where

import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Foldable (asum)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Text.Regex.Applicative

data Segment = A | B | C | D | E | F | G deriving (Enum, Show, Read, Eq, Ord, Bounded)
newtype Digit = Digit { segments :: [Segment] } deriving Show
data Display = Display { allPatterns, reading :: [Digit] } deriving Show

type Input = [Display]

type Regex a = RE Char a

segment :: Regex Segment
segment = asum $ do
  seg <- [minBound..maxBound]
  pure $ seg <$ sym (toLower (head (show seg)))

digit :: Regex Digit
digit = Digit <$> many segment

display :: Regex Display
display = Display <$> replicateM 10 (digit <* sym ' ') <* sym '|' <*> replicateM 4 (sym ' ' *> digit)

input :: Regex Input
input = many (display <* sym '\n')

part1 :: Input -> Int
part1 = go 0
  where go counter [] = counter
        go counter (Display _ reading : more) = go counter' more
          where counter' = counter + (length . filter simpleDigit $ reading)
        simpleDigit = (`elem` [2, 3, 4, 7]) . length . segments

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe [] . (=~ input)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
