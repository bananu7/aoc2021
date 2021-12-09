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
solve a = sum . map ((+1) . (a!)) $ lows
    where
        (sx, sy) = size a

        lows = filter (isLow a) [(x,y) | x <- [0..sx], y <- [0..sy]]

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

