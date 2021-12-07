module Day7 where


parse :: String -> [Int]
parse = map read . words . map replace
    where
        replace ',' = ' '
        replace a = a

solve :: [Int] -> Int
solve xs = foldl (try) 999999999999 [0..length xs]
    where
        try m n =
            if work n < m then
                work n
            else
                m
        work n = sum . map (abs . diff' n) $ xs

diff a b = if a > b then 
            sum [1..(a-b)]
           else
            sum [1..(b-a)]


diff' a b = (l * (l + 1)) `div` 2
    where
        l = abs $ a - b

main_7 = do
    input <- readFile "src/input_7.txt"
    let numbers = parse input
    let answer = solve numbers -- p2 98257206
    print answer