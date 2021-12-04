{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Debug.Trace
import Data.List

-- day 1 part 1
test_1 :: [Int]
test_1 = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

increases :: [Int] -> Int
increases xs = length . filter (>0) . zipWith (-) (tail xs) $ xs

main_1 =
    readFile "src/input_1.txt" >>= 
    return . increases . map read . lines >>=
    print

-- day 1 part 2
increasesWindow3 :: [Int] -> Int
increasesWindow3 xs = length . filter (>0) . zipWith (-) (drop 3 xs) $ xs

main_2 =
    readFile "src/input_1.txt" >>= 
    return . increasesWindow3 . map read . lines >>=
    print

-- day 2 part 1
data Move = Up Int | Down Int | Forward Int

eval :: [Move] -> Int
eval = (\(x,y)->x*y) . foldl evalOne (0,0)
    where
        evalOne (d, f) (Up x) = (d-x, f)
        evalOne (d, f) (Down x) = (d+x, f)
        evalOne (d, f) (Forward x) = (d, f+x)

parseLine :: String -> Move
parseLine line =
    case words line of
        [moveS, vS] -> parseMove moveS $ read vS
        _ -> error "wrong line format"
    where
        parseMove "up" = Up
        parseMove "down" = Down
        parseMove "forward" = Forward

main_3 =
    readFile "src/input_2.txt" >>=
    return . eval . map parseLine . lines >>=
    print


-- day 2 part 3

evalAim :: [Move] -> Int
evalAim = (\(p,d,a) -> p*d) . foldl evalOne (0,0,0)
    where
        evalOne (p, d, a) (Up x)       = (p, d, a-x)
        evalOne (p, d, a) (Down x)     = (p, d, a+x)
        evalOne (p, d, a) (Forward x)  = (p+x, d+x*a, a)

main_4 =
    readFile "src/input_2.txt" >>=
    return . evalAim . map parseLine . lines >>=
    print


-- day 3 part 1

popcount :: [String] -> (Int, [Int])
popcount ls = (length ls, foldl (zipWith add) iv ls)
    where
        add i '0' = i
        add i '1' = i+1
        iv = replicate w 0
        w = length . head $ ls

gV :: Int -> [Int] -> [Bool]
gV n v = map common v
    where
        common i = i > n `div` 2

eV :: Int -> [Int] -> [Bool]
eV n v = map uncommon v
    where
        uncommon 0 = False
        uncommon i = if i > n `div` 2 then False else True

compute :: (Int, [Int]) -> Int
compute (n, v) = ɣ * ε
    where
        ɣ = binToDec $ gV n v
        ε = binToDec $ eV n v
      
binToDec :: [Bool] -> Int
binToDec = foldr (\x y -> fromEnum x + 2*y) 0 . reverse

main_3_1 = 
    readFile "src/input_3.txt" >>=
    print . compute . popcount . lines


-- day 3 part 2
strToBin :: String -> [Bool]
strToBin = map toB
    where toB '0' = False
          toB '1' = True

compute2 :: [String] -> Int
compute2 ls = o2 * co2
  where
    o2 = binToDec . component compO2 $ binLines
    co2 = binToDec . component compCo2 $ binLines
    binLines = map strToBin ls

type Comp = Int -> Int -> Bool

component :: Comp -> [[Bool]] -> [Bool]
component comp = fst . component' comp . map dup
    where
        dup a = (a,a)

component' :: Comp -> [([Bool], [Bool])] -> ([Bool], [Bool])
component' comp [x] = x
component' comp xs = component' comp . map (fmap tail) . filter ((== okBit) . head . snd) $ xs
    where
        okBit = comp (length xs) . length . filter id . map (head . snd) $ xs

compO2 :: Comp
compO2 len x =
    if len `mod` 2 == 0
        then x >= len `div` 2
        else x > len `div` 2

compCo2 :: Comp
compCo2 len x = 
    if len `mod` 2 == 0
        then x < len `div` 2
        else x <= len `div` 2

main_3_2 = 
    readFile "src/input_3.txt" >>=
    print . compute2 . lines

-- day 4 part 1

type BingoRow = [Int]
-- don't need cols, can keep 10x5 because score will be divided by 2
-- because each number will appear twice
data BingoBoard = BingoBoard [BingoRow] deriving (Show, Eq) --10x5

data CrossResult = Victory Int | StillPlaying BingoBoard deriving (Show, Eq)

cross :: Int -> BingoBoard -> CrossResult
cross x (BingoBoard rows) = checkVictory x (BingoBoard rows')
    where
        rows' = map crossRow rows
        crossRow r = filter (/= x) r

checkVictory :: Int -> BingoBoard -> CrossResult
checkVictory x (BingoBoard rows) = 
    if (length . filter null $ rows) > 0 then
        Victory ((*x) . (`div` 2) . sum . map sum $ rows)
    else
        StillPlaying $ BingoBoard rows

readNumbers :: String -> [Int]
readNumbers = map read . words . map commaToSpace
  where
    commaToSpace ',' = ' '
    commaToSpace x = x

-- needs to be 5 strings
readBoard :: [String] -> BingoBoard
readBoard xs = BingoBoard $ numerical ++ transpose numerical
    where
        numerical = map (map read . words) $ xs

splitInput :: [String] -> [[String]]
splitInput [] = []
splitInput xs = (take 5 . drop 1 $ xs) : (splitInput $ drop 6 xs)

play results (x:numbers) = 
    case findVictory results of
        Just score -> score
        Nothing -> play results' numbers
    where
        results' =  map (cross x . getBoard) $ results

        findVictory :: [CrossResult] -> Maybe Int
        findVictory [] = Nothing
        findVictory (Victory x : _) = Just x
        findVictory (StillPlaying _ : r) = findVictory r 

        getBoard (StillPlaying b) = b

main_4_1 = do
    ls <- lines <$> readFile "src/input_4.txt"
    let numbers = readNumbers $ head ls
    let boards = map readBoard . splitInput $ tail ls

    let score = play (map StillPlaying boards) numbers
    print score

-- day 4 part 2

play2 [lastBoard] (x:numbers) = 
    case lastBoard of
        Victory score -> score
        StillPlaying b -> play [cross x b] numbers

play2 results (x:numbers) = play2 results'' numbers
    where
        results'' = filter isPlaying results'
        isPlaying (StillPlaying _) = True
        isPlaying (Victory _) = False

        results' =  map (cross x . getBoard) $ results

        findVictory :: [CrossResult] -> Maybe Int
        findVictory [] = Nothing
        findVictory (Victory x : _) = Just x
        findVictory (StillPlaying _ : r) = findVictory r 

        getBoard (StillPlaying b) = b

main_4_2 = do
    ls <- lines <$> readFile "src/input_4.txt"
    let numbers = readNumbers $ head ls
    let boards = map readBoard . splitInput $ tail ls

    let score = play2 (map StillPlaying boards) numbers
    print score