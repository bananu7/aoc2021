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


--

neighbors :: P -> [P]
neighbors (px,py) = [(px+dx,py+dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx,dy) /= (0,0)]

getTens :: Board -> [P]
getTens b = map fst . filter ((== 10) . snd) . assocs $ b

propagate :: P -> State Board ()
propagate p = return ()

incAll :: Board -> Board
incAll b = b // (map (fmap (+1)) . assocs $ b)

resetFlashes :: Board -> (Int, Board)
resetFlashes b = (numFlashes, b')
    where
        b' = b // (map (fmap toZero) vals)
        toZero x = if x >= 10 then 0 else x
        numFlashes = length . filter ((>= 10) . snd) $ vals
        vals = assocs b

step :: Board -> (Int, Board)
step b = flip runState b $ do
    modify incAll
    tens <- getTens <$> get
    mapM_ propagate tens
    state resetFlashes

main_11_1 = do
    input <- readFile "src/input_11_test.txt"
    let a = toArr input
    printArr a

    let (f, a') = step a
    print f
    printArr a'

    let (f, a'') = step a'
    print f
    printArr a''


--
