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

fixedWidthBinary :: Int -> Parser Int
fixedWidthBinary n = fromBinary <$> replicateM n bit

threeBit :: (Int -> a) -> Parser a
threeBit f = label (f <$> fixedWidthBinary 3) "three bits"

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
subPackets = do
  lt <- lengthType
  case lt of
    NumPackets -> do
      n <- fixedWidthBinary 11
      replicateM n packet
    BitLength -> do
      n <- fixedWidthBinary 15
      bs <- replicateM n bit
      pure $ case parse (many packet) "nested packet" bs of
        Left e -> error $ show e
        Right ps -> ps

lengthType :: Parser LengthType
lengthType = do
  b <- bit
  pure $ case b of
    Zero -> BitLength
    One -> NumPackets

charToBit :: Char -> Bit
charToBit '0' = Zero
charToBit '1' = One

hexToBits :: Char -> [Bit]
hexToBits c = map (boolToBit . testBit (digitToInt c)) [3, 2, 1, 0]
  where boolToBit False = Zero
        boolToBit True = One

type Input = Packet

part1 :: Input -> VersionId
part1 (Packet (Versioned v contents)) = v + case contents of
                                              LiteralPacket _ -> 0
                                              OperatorPacket _ subPackets -> sum . map part1 $ subPackets

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare text = case parse packet "stdin" . (>>= hexToBits) . filter (/= '\n') $ text of
  Left e -> error (show e)
  Right p -> p

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
