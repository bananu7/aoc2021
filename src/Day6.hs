module Day6 (main_6_1) where

import qualified Data.Map.Strict as M

parse :: String -> [Int]
parse = map read . words . map replace
    where
        replace ',' = ' '
        replace a = a

createStart :: [Int] -> M.Map Int Int
createStart = foldl (\m i -> M.insertWith (+) i 1 m) (M.fromList iv)
    where
        iv = zip [0..8] (repeat 0)

eval :: Int -> M.Map Int Int -> M.Map Int Int
eval 0 m = m
eval n m = eval (n-1) m'
    where
        m' = M.fromList [(0, n0), (1, n1), (2, n2), (3, n3), (4, n4), (5, n5), (6, n6), (7, n7), (8, n8)]

        n8 = m M.! 0
        n7 = m M.! 8
        n6 = m M.! 7 + m M.! 0
        n5 = m M.! 6
        n4 = m M.! 5
        n3 = m M.! 4
        n2 = m M.! 3
        n1 = m M.! 2
        n0 = m M.! 1

result :: M.Map Int Int -> Int
result = sum . M.elems

main_6_1 = do
    input <- readFile "src/input_6.txt"
    let numbers = parse input

    let start = createStart numbers
    {-
    print $ eval 0 start
    print $ eval 1 start
    print $ eval 2 start
    print $ eval 3 start
    print $ eval 4 start
    print $ eval 5 start
    -}

    -- print . result $ eval 18 start
    print . result $ eval 80 start
    print . result $ eval 256 start


