{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad.ST
import Data.Foldable (for_)
import Data.Char (digitToInt)
import Data.Ord (comparing)
import Data.List (minimumBy)
import Data.STRef
import Data.Array as A
import Data.Map as M
import Data.Set as S

newtype Risk = Risk Int deriving (Eq, Ord, Num, Show, Bounded)
data Coord = Coord { _x, _y :: Int } deriving (Eq, Ord, Show, Ix)

lowestRisk :: Array Coord Risk -> Risk
lowestRisk a = runST $ do
  let (start, end) = bounds a
  q <- newSTRef (S.fromList (indices a))
  dist <- newSTRef (M.singleton start 0)
  let getDist n = M.findWithDefault maxBound n <$> readSTRef dist
  let search = do
        opens <- S.toList <$> readSTRef q
        weighted <- traverse (\v -> do
                                 d <- getDist v
                                 pure (v, d))
                    opens
        let (u, _weight) = minimumBy (comparing snd) weighted
        modifySTRef q $ S.delete u
        for_ (neighbors u) $ \v -> do
          alt <- (+ (a A.! v)) <$> getDist u
          modifySTRef dist (M.insertWith min v alt)
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

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare input = let ls = lines input
                in listArray (Coord 1 1, Coord (length ls) (length (head ls))) $
                             Prelude.map (Risk . digitToInt) (concat ls)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
