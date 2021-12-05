module Day5 (main_5_1, main_5_2) where

import Data.Array
import Control.Monad.ST
import Data.STRef
import Data.Char (isDigit)

import Debug.Trace
-- array                   :: (Ix a) => (a,a) -> [(a,b)] -> Array a b

type Point = (Int, Int)
type Seabed = Array Point Int

data Line = HorizontalLine (Int, Int) Int | VerticalLine Int (Int, Int) | DiagonalLine deriving (Eq, Show)

showSeabed :: Seabed -> String
showSeabed = unlines . map (concat . map show) . splitn . elems

splitn [] = []
splitn xs = (take 11 $ xs) : (splitn $ drop 11 xs)


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

emptySeabed :: Point -> Seabed
emptySeabed (sx, sy) = array ((0,0), (sx, sy)) [((x,y), 0) | x <- [0..sx], y <- [0..sy]]

mark :: Line -> Seabed -> Seabed
mark (HorizontalLine (x1,x2) y) b = markPoints [(x,y) | x <- [x1..x2]] b
mark (VerticalLine x (y1, y2)) b = markPoints [(x,y) | y <- [y1..y2]] b
mark DiagonalLine b = b

markPoints :: [Point] -> Seabed -> Seabed
markPoints ps b0 = foldl (\b p -> markPoint p b) b0 ps

markPoint :: Point -> Seabed -> Seabed
markPoint p b = let v = b!p in b // [(p, v + 1)]

count :: Seabed -> Int
count = length . filter (>= 2) . elems

markM bed l = do
    b <- readSTRef bed
    let b' = mark l b
    writeSTRef bed b'

solve :: [Line] -> Int
solve ls = runST $ do 
    bed <- newSTRef $ emptySeabed (1000,1000)

    mapM_ (markM bed) ls

    --answer <- count . (\b -> trace (showSeabed b) b) <$> readSTRef bed
    --answer <- count <$> readSTRef bed

    return 42

main_5_1 = do
    input <- lines <$> readFile "src/input_5.txt"
    let ls = map parse input
    --print ls
    let answer = solve ls
    print answer

main_5_2 :: IO ()
main_5_2 = return ()