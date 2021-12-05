module Day5 (main_5_1, main_5_2) where

import Data.Array
import Control.Monad.ST
import Data.STRef
import Data.Char (isDigit)
-- array                   :: (Ix a) => (a,a) -> [(a,b)] -> Array a b

type Point = (Int, Int)
type Seabed = Array Point Int

data Line = HorizontalLine (Int, Int) Int | VerticalLine Int (Int, Int) deriving (Eq, Show)

parse :: String -> Line
parse s = 
    case numbers of
        [x1, y1, x2, y2] -> if y1 == y2 then HorizontalLine (x1,x2) y1 else VerticalLine x1 (y1,y2)
    where
        numbers :: [Int]
        numbers = map read . words . map symbolToSpace $ s

        symbolToSpace x = if isDigit x then x else ' '

emptySeabed :: Point -> Seabed
emptySeabed size = array ((0,0), size) []

solve :: [Line] -> Int
solve ls = runST $ do 
    bed <- newSTRef emptySeabed

    return 42

main_5_1 = do
    input <- lines <$> readFile "src/input_5_test.txt"
    let ls = map parse input
    print ls
    let answer = solve ls
    print answer

main_5_2 :: IO ()
main_5_2 = return ()