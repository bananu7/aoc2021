module Day1 (main_1, main_2) where

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
