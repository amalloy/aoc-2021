module Main where

import Control.Arrow ((&&&))
import Control.Monad ((>=>))

-- To find the nearest leaf on the left:
-- 1. Go up until you traverse backwards across a right-child link
-- 2. Go left once
-- 3. Go right as far as you can

data Number = Simple Int | Pair Number Number
-- Number a = a + (Number a) * (Number a)
-- X(a) = a + X(a) ^ 2
-- X'(a) = 1 + 2 * X'(a)
-- data Number' a = Nil | NumberL' (Number a) (Number' a) | NumberR' (Number a) (Number' a)
data Dir = L | R
type NumContext = [(Number, Dir)]
type NumZipper = (Number, NumContext)

up :: NumZipper -> Maybe NumZipper
up (_n, []) = Nothing -- at the root
up (l, ((r, L):ctx)) = Just (Pair l r, ctx)
up (r, ((l, R):ctx)) = Just (Pair l r, ctx)

left :: NumZipper -> Maybe NumZipper
left (Simple _, _) = Nothing
left (Pair l r, ctx) = Just (l, (r, L):ctx)

right :: NumZipper -> Maybe NumZipper
right (Simple _, _) = Nothing
right (Pair l r, ctx) = Just (r, (l, R):ctx)

next :: NumZipper -> Maybe NumZipper
next (Pair l r, ctx) = Just (l, (r, R):ctx)
next (l, ((r, L):ctx)) = Just (r, (l, R):ctx)
next (r, ((l, R):ctx)) = case ctx of
  [] -> Nothing
  ((r', L):ctx') ->
  ((l', R):ctx') -> undefinde

needsExploding :: NumZipper -> Bool
needsExploding (Simple n, _) = False
needsExploding node@(Pair _ _, _) = case upFourTimes node of
  Nothing -> False
  Just _ -> True
  where upFourTimes = up >=> up >=> up >=> up

type Input = [String]

part1 :: Input -> ()
part1 = const ()

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
