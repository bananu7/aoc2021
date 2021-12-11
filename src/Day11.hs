module Day11 where

import Data.Array.Unboxed
import Control.Monad.State
import Data.Char
import Data.List

type P = (Int, Int)
type Board = UArray P Int

toArr :: String -> UArray P Int
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

--

neighbors :: P -> [P]
neighbors (px,py) = [(px+dx,py+dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx,dy) /= (0,0)]

getTens :: Board -> [P]
getTens b = map fst . filter ((== 10) . snd) . assocs $ b

propagate :: P -> State Board ()
propagate p = return ()

incAll :: State Board ()
incAll = return ()

resetFlashes :: State Board Int
resetFlashes = return 0

step :: Board -> (Int, Board)
step b = flip runState b $ do
    incAll
    tens <- getTens <$> get
    mapM_ propagate tens
    resetFlashes

main_9_1 = do
    input <- readFile "src/input_9.txt"
    let a = toArr input
    return ()


