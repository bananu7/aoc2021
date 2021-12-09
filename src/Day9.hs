module Day9 where

import Data.Array.Unboxed
import Data.Char
import Data.List

type Board = UArray (Int, Int) Int

toArr :: String -> UArray (Int, Int) Int
toArr i = listArray ((0,0), (szX-1, szY-1)) (concat . transpose$ nums)
    where
        szX = length . head $ nums
        szY = length nums
        nums =  map (map digitToInt) . lines $ i

size a = 
    let
        (_, (sx,sy)) = bounds a
    in
        (sx,sy)

printArr :: Board -> IO ()
printArr a = do
    let (sx,sy) = size a
    mapM_ (printRow sx) $ [0..sy]
    where
        printRow sx y = do
            mapM_ (\x -> printChar x y) [0..sx]
            putStrLn ""
        printChar x y =
            putStr . show $ a ! (x, y)

solve :: Board -> Int
solve a = sum . map ((+1) . (a!)) . lows $ a

lows a = filter (isLow a) (indices a)

isLow a (x,y) = all (==True) . map check $ [(x,y-1), (x-1,y), (x,y+1), (x+1,y)]
    where
        v = a ! (x,y)
        check p =
            if not $ (bounds a) `inRange` p then
                True
            else
                v < a ! p

main_9_1 = do
    input <- readFile "src/input_9.txt"
    let a = toArr input
    print $ solve a


-- part 2

up (px,py) = (px,py-1)
down (px,py) = (px,py+1)
left (px,py) = (px-1, py)
right (px,py) = (px+1, py)


basinPoints a p = nub $ p : (concat . map check $ [up p, down p, left p, right p])
    where
        check p' = 
            if not $ (bounds a) `inRange` p' then
                []
            else
                if (not $ a!p' == 9) && a!p' > a!p then basinPoints a p' else []

solve2 a = product . take 3 . reverse . sort . map length . map (basinPoints a) . lows $ a


-- 540540 too low

main_9_2 = do
    input <- readFile "src/input_9_test.txt"
    let a = toArr input
    print $ solve2 a
    print $ reverse . sort . map length . map (basinPoints a) . lows $ a
