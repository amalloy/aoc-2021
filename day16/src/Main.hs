{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Bits (testBit)
import Data.Bool (bool)
import Data.Char (digitToInt)
import Data.List (foldl')
import Control.Monad (guard, replicateM)

import Control.Arrow ((&&&))

import Text.Parsec
import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)

type Val = Integer
data Bit = Zero | One deriving (Show, Eq)
newtype VersionId = VersionId Val deriving (Show, Num)
newtype PacketType = PacketType Val deriving (Show, Num, Eq)
data LengthType = BitLength | NumPackets deriving (Show)
newtype Literal = Literal Val deriving (Show, Num)
data Packet = LiteralPacket VersionId Literal | OperatorPacket VersionId PacketType [Packet] deriving (Show)
makeBaseFunctor ''Packet

type Parser a = Parsec [Bit] () a

fromBinary :: [Bit] -> Val
fromBinary = foldl' nextBit 0
  where nextBit acc Zero = 2 * acc
        nextBit acc One = 2 * acc + 1

packet :: Parser Packet
packet = flip label "packet" $ do
  v <- versionId
  t <- packetType
  if t == literalPacketType
    then LiteralPacket v <$> literal
    else OperatorPacket v t <$> subPackets
  where literalPacketType = 4

fixedWidthBinary :: Int -> Parser Val
fixedWidthBinary n = fromBinary <$> replicateM n bit <?> show n ++ "-bit binary number"

versionId :: Parser VersionId
versionId = VersionId <$> fixedWidthBinary 3 <?> "version ID"

packetType :: Parser PacketType
packetType = PacketType <$> fixedWidthBinary 3 <?> "packet type"

bit :: Parser Bit
bit = anyToken <?> "bit"

literal :: Parser Literal
literal = flip label "literal" $ do
  prefix <- concat <$> many (chunkPrefixedWith One)
  suffix <- chunkPrefixedWith Zero
  pure . Literal . fromBinary $ prefix ++ suffix

chunkPrefixedWith :: Bit -> Parser [Bit]
chunkPrefixedWith expected = flip label ("chunk prefixed with " ++ show expected) . try $ do
  header <- bit
  guard $ header == expected
  replicateM 4 bit

subPackets :: Parser [Packet]
subPackets = do
  lt <- lengthType
  case lt of
    NumPackets -> do
      n <- fixedWidthBinary 11
      replicateM (fromIntegral n) packet
    BitLength -> do
      n <- fixedWidthBinary 15
      bs <- replicateM (fromIntegral n) bit
      pure $ case parse (many packet) "nested packet" bs of
        Left e -> error $ show e
        Right ps -> ps

lengthType :: Parser LengthType
lengthType = do
  b <- bit
  pure $ case b of
    Zero -> BitLength
    One -> NumPackets

hexToBits :: Char -> [Bit]
hexToBits c = map (boolToBit . testBit (digitToInt c)) [3, 2, 1, 0]
  where boolToBit False = Zero
        boolToBit True = One

part1 :: Packet -> VersionId
part1 = cata go
  where go (LiteralPacketF ver _val) = ver
        go (OperatorPacketF ver _type subPackets) = ver + sum subPackets

part2 :: Packet -> Val
part2 = cata go
  where go (LiteralPacketF _version (Literal v)) = v
        go (OperatorPacketF _version (PacketType op) packets) = apply op packets
        apply 0 = sum
        apply 1 = product
        apply 2 = minimum
        apply 3 = maximum
        apply 5 = comparison (>)
        apply 6 = comparison (<)
        apply 7 = comparison (==)
        apply n = error $ "Unknown operation " ++ show n
        comparison f [a, b] = bool 0 1 $ f a b
        comparison f xs = error $ "Expected exactly two packets, but got " ++ show xs

prepare :: String -> Packet
prepare text = case parse packet "stdin" . (>>= hexToBits) . filter (/= '\n') $ text of
  Left e -> error (show e)
  Right p -> p

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
