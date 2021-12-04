module Day3 (main_3_1, main_3_2) where

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

