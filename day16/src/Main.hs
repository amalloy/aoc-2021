{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.List (foldl')

import Control.Arrow ((&&&))

import Text.Parsec

data Bit = Zero | One deriving (Show)
newtype VersionId = VersionId Int deriving (Show, Num)
newtype PacketType = PacketType Int deriving (Show, Num)
data LengthType = BitLength | NumPackets deriving (Show)
newtype Literal = Literal Int deriving (Show, Num)
data Versioned a = Versioned VersionId a deriving (Show)
data PacketContents = LiteralPacket Literal | OperatorPacket PacketType [Packet] deriving (Show)
newtype Packet = Packet (Versioned PacketContents) deriving (Show)

type Parser a = Parsec [Bit] () a

fromBinary :: [Bit] -> Int
fromBinary = foldl' nextBit 0
  where nextBit acc Zero = 2 * acc
        nextBit acc One = 2 * acc + 1

type Input = [String]

part1 :: Input -> ()
part1 = const ()

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
