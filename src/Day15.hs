{-# LANGUAGE FlexibleContexts #-}

module Day15 where

import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.State
import Control.Monad.ST
import Data.STRef
import Data.Char
import Data.List
import Debug.Trace
import qualified Data.PQueue.Min as PQ

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
            putStr . (++ "") . show $ a ! (x, y)

dynPath :: Board -> Board
dynPath a = dp (sx,sy) e'
    where 
        (sx,sy) = (size a)
        e = listArray ((0,0), (sx, sy)) (replicate ((sx+1)*(sy+1)) 9999999999)
        e' = e // [ ((sx, sy), a ! (sx, sy)) ]

        dp :: P -> Board -> Board
        dp (0,0) na = na
        dp (x,y) na = 
            checkPoint (x-1, y) . checkPoint (x, y-1) . checkPoint (x+1, y) . checkPoint (x, y+1)  $ na
            where
                checkPoint (px,py) na = 
                    if not $ (bounds na) `inRange` (px,py) then
                        na
                    else
                        if (na ! (px, py) > a ! (px, py) + na ! (x,y))
                        then
                            let
                                na' = na // [ ((px,py), (a ! (px, py) + na ! (x,y))) ]
                            in
                                dp (px, py) na'
                        else
                            na

dynPathS :: Board -> Board
dynPathS a = runSTUArray $ do
    er <- newListArray ((0,0), (sx, sy)) (replicate ((sx+1)*(sy+1)) 9999999999)

    writeArray er (sx, sy) (a ! (sx, sy))

    dp (sx,sy) er
    return er
    where 
        (sx,sy) = (size a)

        dp (0,0) er = return ()
        dp (x,y) er = do
            checkPoint (x-1, y)
            checkPoint (x, y-1)
            checkPoint (x+1, y)
            checkPoint (x, y+1)
            where
                checkPoint (px,py) = 
                    if not $ (bounds a) `inRange` (px,py) then
                        return ()
                    else do
                        pv <- readArray er (px,py)
                        v <- readArray er (x,y)
                        if (pv > a ! (px, py) + v)
                        then do
                            writeArray er (px,py) (a ! (px, py) + v)
                            dp (px,py) er
                        else
                            return ()

dynPathRS :: Board -> Board
dynPathRS a = runST $ do
    --er <- newListArray ((0,0), (sx, sy)) (replicate ((sx+1)*(sy+1)) 9999999999)
    er <- newArray ((0,0), (sx, sy)) 9999999999 :: ST s (STUArray s P Int)

    q <- newSTRef $ (PQ.empty :: PQ.MinQueue (Int, P))

    writeArray er (sx, sy) (a ! (sx, sy))

    dp (sx,sy) q er
    freeze er
    where 
        (sx,sy) = (size a)

        dp (0,0) q er = return ()
        dp (x,y) q er = do
            checkPoint (x-1, y)
            checkPoint (x, y-1)
            checkPoint (x+1, y)
            checkPoint (x, y+1)

            (_, p') <- do
                Just pp <- PQ.getMin <$> readSTRef q
                modifySTRef q $ PQ.deleteMin
                return pp

            dp p' q er
            where
                checkPoint p = 
                    if not $ (bounds a) `inRange` p then
                        return ()
                    else do
                        pv <- readArray er p
                        v <- readArray er (x,y)
                        let nv = a!p + v
                        if (pv > nv)
                        then do
                            writeArray er p nv
                            modifySTRef q $ PQ.insert (nv, p)
                        else
                            return ()


shortestPath :: Board -> Int
shortestPath a = a' ! (0,0)
    where a' = dynPath a



main_15_1 = do
    input <- readFile "src/input_15.txt"
    let a = toArr input
 
    let a' = dynPathRS a

    --printArr a
    --printArr a'
    print $ a' ! (0,0) - a ! (0,0)


toArr5 :: String -> UArray P Int
toArr5 i = listArray ((0,0), (mX-1,mY-1)) (concat . transpose$ rows5)
    where
        mX = szX * 5
        mY = szY * 5
        szX = length . head $ nums
        szY = length nums

        --rows5  = concat . replicate 5 $ numsLines5
        rows5 = concat . map (\i -> map (\row -> addWrap i row) $ numsLines5) $ [0,1,2,3,4] 

        numsLines5 = map (\row -> concat . map (\i -> addWrap i row) $ [0,1,2,3,4] ) nums
        addWrap i r = map (wrap9 . (+i)) r
        wrap9 x = if x > 9 then x - 9 else x
        nums =  map (map digitToInt) . lines $ i


main_15_2 = do
    input <- readFile "src/input_15.txt"
    let a = toArr5 input
 
    --printArr a 

    let a' = dynPathRS a

    print $ a' ! (0,0) - a ! (0,0)





