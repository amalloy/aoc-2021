{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Bits (testBit)
import Data.Bool (bool)
import Data.Char (digitToInt)
import Data.List (foldl')
import Control.Monad (guard, replicateM)

import Control.Arrow ((&&&))

import Text.Parsec

type Val = Integer
data Bit = Zero | One deriving (Show, Eq)
newtype VersionId = VersionId Val deriving (Show, Num)
newtype PacketType = PacketType Val deriving (Show, Num, Eq)
data LengthType = BitLength | NumPackets deriving (Show)
newtype Literal = Literal Val deriving (Show, Num)
data Versioned a = Versioned VersionId a deriving (Show)
data PacketContents = LiteralPacket Literal | OperatorPacket PacketType [Packet] deriving (Show)
newtype Packet = Packet (Versioned PacketContents) deriving (Show)

type Parser a = Parsec [Bit] () a

fromBinary :: [Bit] -> Val
fromBinary = foldl' nextBit 0
  where nextBit acc Zero = 2 * acc
        nextBit acc One = 2 * acc + 1

packet :: Parser Packet
packet = Packet <$> versioned packetContents <?> "packet"

versioned :: Parser a -> Parser (Versioned a)
versioned p = Versioned <$> versionId <*> p

fixedWidthBinary :: Int -> Parser Val
fixedWidthBinary n = fromBinary <$> replicateM n bit <?> show n ++ "-bit binary number"

versionId :: Parser VersionId
versionId = VersionId <$> fixedWidthBinary 3 <?> "version ID"

packetType :: Parser PacketType
packetType = PacketType <$> fixedWidthBinary 3 <?> "packet type"

bit :: Parser Bit
bit = anyToken <?> "bit"

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

foldPacket :: Packet -> Val
foldPacket (Packet (Versioned _ contents)) = case contents of
  LiteralPacket (Literal v) -> v
  OperatorPacket (PacketType op) packets ->
    apply op (map foldPacket packets)

apply :: Val -> [Val] -> Val
apply 0 = sum
apply 1 = product
apply 2 = minimum
apply 3 = maximum
apply 5 = comparison (>)
apply 6 = comparison (<)
apply 7 = comparison (==)
apply n = error $ "Unknown operation " ++ show n

comparison :: (Val -> Val -> Bool) -> [Val] -> Val
comparison f [a, b] = bool 0 1 $ f a b
comparison f xs = error $ "Expected exactly two packets, but got " ++ show xs

part1 :: Packet -> VersionId
part1 (Packet (Versioned v contents)) = v + case contents of
                                              LiteralPacket _ -> 0
                                              OperatorPacket _ subPackets -> sum . map part1 $ subPackets

part2 :: Packet -> Val
part2 = foldPacket

prepare :: String -> Packet
prepare text = case parse packet "stdin" . (>>= hexToBits) . filter (/= '\n') $ text of
  Left e -> error (show e)
  Right p -> p

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
