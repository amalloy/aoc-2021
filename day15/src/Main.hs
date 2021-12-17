{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad.ST
import Control.Monad (when)
import Data.Foldable (for_)
import Data.Char (digitToInt)
import Data.Ord (comparing)
import Data.List (minimumBy)

import Data.STRef

import Data.Heap (Heap, Entry(..))
import qualified Data.Heap as H
import Data.Array as A
import qualified Data.Map as M
import qualified Data.Set as S

newtype Risk = Risk Int deriving (Eq, Ord, Num, Show, Bounded)
data Coord = Coord { _x, _y :: Int } deriving (Eq, Ord, Show, Ix)

lowestRisk :: Array Coord Risk -> Risk
lowestRisk a = runST $ do
  let b@(start, end) = bounds a
  q <- newSTRef (H.singleton (Entry 0 start))
  dist <- newSTRef (M.singleton start 0)
  let getDist n = M.findWithDefault maxBound n <$> readSTRef dist
  let search = do
        Entry risk u <- H.minimum <$> readSTRef q
        modifySTRef q H.deleteMin
        tentativeDistance <- getDist u
        when (risk == tentativeDistance) $ do
          for_ (neighbors u) $ \v -> do
            when (inRange b v) $ do
              alt <- (+ (a A.! v)) <$> getDist u
              oldDist <- getDist v
              when (alt < oldDist) $ do
                modifySTRef dist (M.insert v alt)
                modifySTRef q (H.insert (Entry alt v))
        if u == end
          then getDist u
          else search
  search

neighbors :: Coord -> [Coord]
neighbors (Coord x y) = [ Coord (x + 1) y
                        , Coord (x - 1) y
                        , Coord x (y + 1)
                        , Coord x (y - 1)
                        ]

type Input = Array Coord Risk

part1 :: Input -> Risk
part1 = lowestRisk

expand :: Array Coord Risk -> Array Coord Risk
expand a = array (Coord 1 1, Coord (w * 5) (h * 5)) $ do
  (Coord x y, Risk r) <- A.assocs a
  dx <- [0..4]
  dy <- [0..4]
  pure (Coord (x + (w * dx)) (y + (w * dy)), Risk . nineWrap $ r + dx + dy)
  where (Coord 1 1, Coord w h) = bounds a
        nineWrap n = ((n - 1) `mod` 9) + 1

part2 :: Input -> Risk
part2 = lowestRisk . expand

prepare :: String -> Input
prepare input = let ls = lines input
                in listArray (Coord 1 1, Coord (length ls) (length (head ls))) $
                             Prelude.map (Risk . digitToInt) (concat ls)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
