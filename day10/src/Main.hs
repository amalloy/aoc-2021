module Main where

import Control.Arrow ((&&&))
import Data.Maybe (mapMaybe)

data Closer = Closer { _opener :: Char, _score :: Int } deriving Show
data ParseResult = Correct | Corrupted Int | Incomplete [Char] deriving Show

closer :: Char -> Maybe Closer
closer = flip lookup
  [ (')', Closer '(' 3)
  , (']', Closer '[' 57)
  , ('}', Closer '{' 1197)
  , ('>', Closer '<' 25137)
  ]

type Input = [String]

corruptedScore :: [Char] -> ParseResult
corruptedScore = go []
  where go [] [] = Correct
        go s [] = Incomplete s
        go stack (i:input) = case closer i of
          Nothing -> go (i:stack) input
          Just (Closer expected score) -> case stack of
            [] -> error "closer with no matching open at all"
            s:more | expected == s -> go more input
                   | otherwise -> Corrupted score

part1 :: Input -> Int
part1 = sum . mapMaybe (getScore . corruptedScore)
  where getScore (Corrupted n) = Just n
        getScore _ = Nothing

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
