module Lib where

import Debug.Trace

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
