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
data Packet = Packet4 Version Int | PacketOther Len Version [Packet] deriving (Show, Eq)

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
        _ -> packetOther packVer

packetOther :: Version -> Parser Packet
packetOther v = do
    lengthTypeId <- bit
    if lengthTypeId == False then do
        len <- numN 15
        subs <- many $ packet
        return $ PacketOther (BinLen len) v subs
    else do
        numSubs <- numN 11
        subs <- many $ packet
        return $ PacketOther (PacketLen numSubs) v subs

zeroes = many $ char '0'

packet4 :: Version -> Parser Packet
packet4 v = do
    numBits <- many (char '1' >> rept 4)
    lastNumBits <- char '0' >> rept 4
    zeroes
    return $ Packet4 v (bitsToInt $ concat numBits ++ lastNumBits)

test_1 = inputToBits "D2FE28"

parse = parseTest (packet <* eof)
parse4 = parseTest (packet4 42 <* eof)
parseTriple = parseTest (triple >> triple <* eof)

