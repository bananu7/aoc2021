module Day5 (main_5_1, main_5_2, rangeOverlap) where

import Data.Array
import Control.Monad.ST
import Data.STRef
import Data.Char (isDigit)

import Debug.Trace
-- array                   :: (Ix a) => (a,a) -> [(a,b)] -> Array a b

type Point = (Int, Int)
data Line = HorizontalLine (Int, Int) Int | VerticalLine Int (Int, Int) | DiagonalLine deriving (Eq, Show)

parse :: String -> Line
parse s = 
    case numbers of
        [x1, y1, x2, y2] -> 
            if y1 == y2 
                then HorizontalLine (min x1 x2, max x1 x2) y1 
            else if x1 == x2
                then VerticalLine x1 (min y1 y2, max y1 y2)
            else
                DiagonalLine
    where
        numbers :: [Int]
        numbers = map read . words . map symbolToSpace $ s

        symbolToSpace x = if isDigit x then x else ' '

countOverlap :: Line -> Line -> Int
countOverlap (HorizontalLine (x1, x2) y1) (HorizontalLine (x3, x4) y2) = 
    if y1 /= y2 then 0 else rangeOverlap x1 x2 x3 x4

countOverlap (VerticalLine x1 (y1, y2)) (VerticalLine x2 (y3, y4)) = 
    if x1 /= x2 then 0 else rangeOverlap y1 y2 y3 y4

countOverlap (VerticalLine x (y1, y2)) (HorizontalLine (x1, x2) y) = 
    if x `between` (x1,x2) && y `between` (y1, y2) then 1 else 0

countOverlap (HorizontalLine (x1, x2) y) (VerticalLine x (y1, y2)) = 
    if x `between` (x1,x2) && y `between` (y1, y2) then 1 else 0

countOverlap _ _ = 0

{-
genOverlap :: Line -> Line -> [Point]
genOverlap (HorizontalLine (x1, x2) y1) (HorizontalLine (x3, x4) y2) = 
    if y1 /= y2 then 0 else genRangeOverlap x1 x2 x3 x4

genOverlap (VerticalLine x1 (y1, y2)) (VerticalLine x2 (y3, y4)) = 
    if x1 /= x2 then 0 else genPointsVertical x1 y1 y2 y3 y4

genOverlap (VerticalLine x (y1, y2)) (HorizontalLine (x1, x2) y) = 
    if x `between` (x1,x2) && y `between` (y1, y2) then [(x, y)]

genOverlap (HorizontalLine (x1, x2) y) (VerticalLine x (y1, y2)) = 
    if x `between` (x1,x2) && y `between` (y1, y2) then [(x, y)] else []

genPointsVertical 

genOverlap _ _ = []
-}

between a (b,c) = a >= b && a <= c
rangeOverlap a b c d = 
    if b >= c && b <= d then
        if a < c then b-c+1 else b-a+1
    else
        if a >= c && a <= d then
            d-a+1
        else
            if d <= b && c >= a then
                d-c+1
            else
                0

solve :: [Line] -> Int
solve [] = 0
solve (l:ls) = sum (map (countOverlap l) ls) + solve ls

main_5_1 = do
    input <- lines <$> readFile "src/input_5.txt"
    let ls = map parse input
    let answer = solve ls
    print answer

main_5_2 :: IO ()
main_5_2 = return ()