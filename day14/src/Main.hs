module Main where

import Control.Arrow ((&&&))
import Data.Char (isUpper)
import Data.Maybe (fromMaybe)

import qualified Data.Map.Strict as M
import Text.Regex.Applicative

newtype Element = Element Char deriving (Eq, Ord, Show)
data Pair a = Pair a a deriving (Eq, Ord, Show)
data Rule a = Rule (Pair a) a deriving (Show)

type Polymer a = M.Map (Pair a) Integer
data Input = Input { _elements :: Polymer Element, _transition :: Pair Element -> Element ,
                     _first :: Element}

step :: Input -> Input
step (Input m f i) = Input m' f i
  where m' = M.fromListWith (+) $ do
          (p@(Pair a b), n) <- M.assocs m
          let between = f p
          [(Pair a between, n), (Pair between b, n)]

score :: Input -> Integer
score (Input m _ i) = maximum quantities - minimum quantities
  where quantities = M.elems (M.insertWith (+) i 1 (M.fromListWith (+) $ do
                                                       (Pair _a b, n) <- M.assocs m
                                                       [(b, n)]))

solve :: Int -> Input -> Integer
solve n = score . (!! n) . iterate step

part1 :: Input -> Integer
part1 = solve 10

part2 :: Input -> Integer
part2 = solve 40

extraCredit = solve 200

prepare :: String -> Input
prepare = fromMaybe (Input mempty undefined undefined) . (=~ input)
  where input = mkInput <$> many element <* string "\n\n" <*> transition
          where mkInput es f = Input (mkPolymer es) f (head es)
                mkPolymer es = M.fromListWith (+) $ zipWith entry es (tail es)
                entry a b = (Pair a b, 1)
        element = Element <$> psym isUpper
        transition = mkTransition <$> many rule
          where mkTransition rules = ((M.fromList [(p, e) | (Rule p e) <- rules]) M.!)
        rule = Rule <$> pair <* string " -> " <*> element <* sym '\n'
        pair = Pair <$> element <*> element

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2 &&& extraCredit) . prepare
