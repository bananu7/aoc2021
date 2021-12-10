module Day10 where

import Data.Maybe

parseLine :: String -> Maybe Char
parseLine = parseLine' []

parseLine' ('<':a) ('>':b) = parseLine' a b
parseLine' ('[':a) (']':b) = parseLine' a b
parseLine' ('{':a) ('}':b) = parseLine' a b
parseLine' ('(':a) (')':b) = parseLine' a b

parseLine' a ('<':b) = parseLine' ('<':a) b
parseLine' a ('[':b) = parseLine' ('[':a) b
parseLine' a ('{':b) = parseLine' ('{':a) b
parseLine' a ('(':b) = parseLine' ('(':a) b

parseLine' _ ('>':b) = Just '>'
parseLine' _ (']':b) = Just ']'
parseLine' _ ('}':b) = Just '}'
parseLine' _ (')':b) = Just ')'

parseLine [] [] = Nothing -- clean
parseLine' _ [] = Nothing -- incomplete

score :: Char -> Integer
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

main_10_1 = do
    input <- lines <$> readFile "src/input_10.txt"

    let s = sum . map score . catMaybes . map parseLine $ input

    print s

-- part2