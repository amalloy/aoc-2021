{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Bits (testBit)
import Data.Char (digitToInt)
import Data.List (foldl')
import Control.Monad (guard, replicateM)

import Control.Arrow ((&&&))

import Text.Parsec

data Bit = Zero | One deriving (Show, Eq)
newtype VersionId = VersionId Int deriving (Show, Num)
newtype PacketType = PacketType Int deriving (Show, Num, Eq)
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

packet :: Parser Packet
packet = label (Packet <$> versioned packetContents) "packet"

versioned :: Parser a -> Parser (Versioned a)
versioned p = Versioned <$> versionId <*> p

threeBit :: (Int -> a) -> Parser a
threeBit f = label (f . fromBinary <$> replicateM 3 bit) "three bits"

versionId :: Parser VersionId
versionId = label (threeBit VersionId) "version ID"

packetType :: Parser PacketType
packetType = label (threeBit PacketType) "packet type"

bit :: Parser Bit
bit = label anyToken "bit"

packetContents :: Parser PacketContents
packetContents = do
  t <- packetType
  if t == literalPacketType
    then LiteralPacket <$> literal
    else OperatorPacket t <$> subPackets
  where literalPacketType = 4

literal :: Parser Literal
literal = flip label "literal" $ do
  prefix <- concat <$> many (chunkPrefixedWith One)
  suffix <- chunkPrefixedWith Zero
  pure . Literal . fromBinary $ prefix ++ suffix

chunkPrefixedWith :: Bit -> Parser [Bit]
chunkPrefixedWith expected = flip label "chunk" . try $ do
  header <- bit
  guard $ header == expected
  replicateM 4 bit

subPackets :: Parser [Packet]
subPackets = undefined

charToBit :: Char -> Bit
charToBit '0' = Zero
charToBit '1' = One

hexToBits :: Char -> [Bit]
hexToBits c = map (boolToBit . testBit (digitToInt c)) [3, 2, 1, 0]
  where boolToBit False = Zero
        boolToBit True = One

type Input = [String]

part1 :: Input -> ()
part1 = const ()

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
