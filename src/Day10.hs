module Day10 where

import Data.Maybe
import Data.List

data Result = Clean | Broken Char | Incomplete String

parseLine :: String -> Result
parseLine = parseLine' []

parseLine' ('(':a) (')':b) = parseLine' a b
parseLine' ('[':a) (']':b) = parseLine' a b
parseLine' ('{':a) ('}':b) = parseLine' a b
parseLine' ('<':a) ('>':b) = parseLine' a b

parseLine' a ('(':b) = parseLine' ('(':a) b
parseLine' a ('[':b) = parseLine' ('[':a) b
parseLine' a ('{':b) = parseLine' ('{':a) b
parseLine' a ('<':b) = parseLine' ('<':a) b

parseLine' _ (')':b) = Broken ')'
parseLine' _ (']':b) = Broken ']'
parseLine' _ ('}':b) = Broken '}'
parseLine' _ ('>':b) = Broken '>'

parseLine' [] [] = Clean
parseLine' a  [] = Incomplete a

score :: Char -> Integer
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

catBroken [] = []
catBroken (Broken a : r) = a : catBroken r
catBroken (_:r) = catBroken r

main_10_1 = do
    input <- lines <$> readFile "src/input_10.txt"

    let s = sum . map score . catBroken . map parseLine $ input

    print s


-- part2

catIncomplete [] = []
catIncomplete (Incomplete a : r) = a : catIncomplete r
catIncomplete (_:r) = catIncomplete r

val :: Char -> Int
val '(' = 1
val '[' = 2
val '{' = 3
val '<' = 4

score2 :: String -> Int
score2 = foldl one 0
    where
        one s c = s*5 + val c 

main_10_2 = do
    input <- lines <$> readFile "src/input_10.txt"

    let scores = sort . map score2 . catIncomplete . map parseLine $ input
    let s = head . drop (length scores `div` 2) $ scores

    print s