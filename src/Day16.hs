module Day16 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Numeric (showIntAtBase)
import Data.Char
import Control.Monad

inputToBits :: String -> String
inputToBits = concat . map (pad . (\x -> showIntAtBase 2 intToDigit x "") . digitToInt)
    where
        pad [a,b,c,d] = [a,b,c,d]
        pad x = pad ('0':x)

type Version = Int -- 3bits
data Len = BinLen Int | PacketLen Int deriving (Show, Eq)
type PType = Int

data Packet =
    Packet4 Version Int |
    PacketOther Version PType Len  [Packet] deriving (Show, Eq)

bitsToInt :: String -> Int
bitsToInt = foldr step 0 . reverse
    where step x y = (digitToInt x) + (y * 2)

type Parser = Parsec Void String

bit :: Parser Bool
bit = do
    c <- digitChar
    case c of
        '0' -> return False
        '1' -> return True
        _ -> error "wrong character"

ibit = toI <$> bit
    where
        toI True = 1
        toI False = 0

rept :: Int -> Parser String
rept n = replicateM n digitChar

triple :: Parser Int
triple = bitsToInt <$> rept 3

numN :: Int -> Parser Int
numN n = bitsToInt <$> rept n

packet :: Parser Packet
packet = do
    packVer <- triple
    packId <- triple
    case packId of
        4 -> packet4 packVer
        _ -> packetOther packVer packId

packetOther :: Version -> PType -> Parser Packet
packetOther v t = do
    lengthTypeId <- bit
    if lengthTypeId == False then do
        len <- numN 15
        cs <- rept len
        let Just subs = parseMaybe (many packet) cs
        --subs <- many $ packet
        return $ PacketOther v t (BinLen len) subs
    else do
        numSubs <- numN 11
        subs <- replicateM numSubs packet
        return $ PacketOther v t (PacketLen numSubs) subs

packet4 :: Version -> Parser Packet
packet4 v = do
    numBits <- many (char '1' >> rept 4)
    lastNumBits <- char '0' >> rept 4
    return $ Packet4 v (bitsToInt $ concat numBits ++ lastNumBits)

test_1 = inputToBits "D2FE28"

parsePacket = parseTest (packet)
parseHex = parsePacket . inputToBits

parsePacketM = runParser packet ""
parseHexM = parsePacketM . inputToBits

versionSum :: Packet -> Int
versionSum (Packet4 v _) = v
versionSum (PacketOther v _ _ ps) = v + (sum . map versionSum $ ps)

main_16_1 = do
    s <- readFile "src/input_16.txt"
    let p = parseHexM s
    case p of
        Right p2 -> print $ versionSum p2
        Left e -> print e

--  --------

eval :: Packet -> Int
eval (Packet4 _ i) = i
eval (PacketOther _ 0 _ ps) = sum . map eval $ ps
eval (PacketOther _ 1 _ ps) = product . map eval $ ps
eval (PacketOther _ 2 _ ps) = minimum . map eval $ ps
eval (PacketOther _ 3 _ ps) = maximum . map eval $ ps
eval (PacketOther _ 5 _ [a,b]) = if eval a > eval b then 1 else 0
eval (PacketOther _ 6 _ [a,b]) = if eval a < eval b then 1 else 0
eval (PacketOther _ 7 _ [a,b]) = if eval a == eval b then 1 else 0

main_16_2 = do
    s <- readFile "src/input_16.txt"
    let p = parseHexM s
    case p of
        Right p2 -> print $ eval p2
        Left e -> print e


