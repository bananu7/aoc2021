module Day5 (main_5_1, main_5_2, parse, gen, Point(..), Line(..)) where

import Data.Char (isDigit)
import Data.List (nub, intersect)
import qualified Data.Set as S

import Debug.Trace
-- array                   :: (Ix a) => (a,a) -> [(a,b)] -> Array a b

type Point = (Int, Int)
data Line =
    HorizontalLine (Int, Int) Int |
    VerticalLine Int (Int, Int) |
    DiagonalLine (Int, Int) (Int, Int) deriving (Eq, Show)

parse :: String -> Line
parse s = 
    case numbers of
        [x1, y1, x2, y2] -> 
            if y1 == y2 
                then HorizontalLine (min x1 x2, max x1 x2) y1 
            else if x1 == x2
                then VerticalLine x1 (min y1 y2, max y1 y2)
            else
                if x2 >= x1 then -- go right
                    DiagonalLine (x1, y1) (x2, y2)
                else
                    DiagonalLine (x2, y2) (x1, y1)
    where
        numbers :: [Int]
        numbers = map read . words . map symbolToSpace $ s

        symbolToSpace x = if isDigit x then x else ' '

between a (b,c) = a >= b && a <= c

genOverlap :: Line -> Line -> [Point]
genOverlap (HorizontalLine (x1, x2) y1) (HorizontalLine (x3, x4) y2) = 
    if y1 /= y2 then [] else genPointsHorizontal y1 x1 x2 x3 x4

genOverlap (VerticalLine x1 (y1, y2)) (VerticalLine x2 (y3, y4)) = 
    if x1 /= x2 then [] else genPointsVertical x1 y1 y2 y3 y4

genOverlap (VerticalLine x (y1, y2)) (HorizontalLine (x1, x2) y) = 
    if x `between` (x1,x2) && y `between` (y1, y2) then [(x, y)] else []

genOverlap (HorizontalLine (x1, x2) y) (VerticalLine x (y1, y2)) = 
    if x `between` (x1,x2) && y `between` (y1, y2) then [(x, y)] else []

genOverlap _ _ = []

genPointsVertical x y1 y2 y3 y4 = [(x,y) | y <- [(min y1 y3)..(max y2 y4)], y >= y1 && y >= y3 && y <= y2 && y <= y4]
genPointsHorizontal y x1 x2 x3 x4 = [(x,y) | x <- [(min x1 x3)..(max x2 x4)], x >= x1 && x >= x3 && x <= x2 && x <= x4]

solve :: [Line] -> Int
solve ls = length . nub .  genAll $ ls

genAll :: [Line] -> [Point]
genAll [] = []
genAll (l:ls) = (concat . map (genOverlap l) $ ls)++ genAll ls

main_5_1 = do
    input <- lines <$> readFile "src/input_5.txt"
    let ls = map parse input
    let answer = solve ls
    print answer

-- part 2

gen :: Line -> [Point]
gen (HorizontalLine (x1, x2) y)         = [(x,y) | x <- [x1..x2]]
gen (VerticalLine x (y1, y2))           = [(x,y) | y <- [y1..y2]]
gen (DiagonalLine (x1, y1) (x2, y2))    = zip [x1..x2] (if y2 > y1 then [y1..y2] else [y1, y1-1 .. y2])

genOverlap' :: Line -> Line -> [Point]
genOverlap' (HorizontalLine (x1, x2) y1) (HorizontalLine (x3, x4) y2) = 
    if y1 /= y2 then [] else genPointsHorizontal y1 x1 x2 x3 x4

genOverlap' (VerticalLine x1 (y1, y2)) (VerticalLine x2 (y3, y4)) = 
    if x1 /= x2 then [] else genPointsVertical x1 y1 y2 y3 y4

genOverlap' (VerticalLine x (y1, y2)) (HorizontalLine (x1, x2) y) = 
    if x `between` (x1,x2) && y `between` (y1, y2) then [(x, y)] else []

genOverlap' (HorizontalLine (x1, x2) y) (VerticalLine x (y1, y2)) = 
    if x `between` (x1,x2) && y `between` (y1, y2) then [(x, y)] else []

genOverlap' a b = S.toList $ (S.fromList $ gen a) `S.intersection` (S.fromList $ gen b)

solve' :: [Line] -> Int
solve' ls = length . nub .  genAll' $ ls

genAll' :: [Line] -> [Point]
genAll' [] = []
genAll' (l:ls) = (concat . map (genOverlap' l) $ ls) ++ genAll' ls

main_5_2 = do
    input <- lines <$> readFile "src/input_5.txt"
    let ls = map parse input
    let answer = solve' ls
    print . genAll' $ ls
    print answer

