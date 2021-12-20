{-# LANGUAGE TupleSections #-}

module Day20 where

import qualified Data.Map.Strict as Map
import Data.Array.Unboxed
import Data.List
import Debug.Trace

type Enhancer = Map.Map Int Bool
type P = (Int, Int)
type Img = UArray P Bool

cToB '#' = True
cToB '.' = False

parseEnhancer :: String -> Enhancer
parseEnhancer s = foldl add (Map.empty) annotated
    where
        add m (i,c) = Map.insert i (cToB c) m
        annotated = zip [0..] s

parseImage :: [String] -> UArray P Bool
parseImage i = listArray ((0,0), (szX-1, szY-1)) (concat . transpose $ nums)
    where
        szX = length . head $ nums
        szY = length nums
        nums = map (map cToB) i

printImg :: Img -> IO ()
printImg a = mapM_ (printRow sx) $ [zy..sy]
    where
        printRow sx y = do
            mapM_ (\x -> printChar x y) [zx..sx]
            putStrLn ""
        printChar x y =
            putStr . bToC $ a ! (x, y)
        bToC True = "#"
        bToC False = "."
        ((zx,zy), (sx,sy)) = bounds a

bitsToInt :: [Bool] -> Int
bitsToInt = foldr step 0 . reverse
    where step x y = (if x then 1 else 0) + (y * 2)

enhance :: Bool -> Enhancer -> Img -> Img
enhance d e a = array newBounds (trace "enhancing" newImg)
    where
        newImg = map newPixel $ range newBounds

        newPixel p = (p, newPixelVal p)

        newPixelVal p = (e Map.!) . bitsToInt . oldBvec $ p

        oldBvec (x,y) =
            [ oldPixel (x-1,y-1), oldPixel (x  ,y-1), oldPixel (x+1,y-1)
            , oldPixel (x-1,y  ), oldPixel (x  ,y  ), oldPixel (x+1,y  )
            , oldPixel (x-1,y+1), oldPixel (x  ,y+1), oldPixel (x+1,y+1)
            ]


        oldPixel p =
            if not $ (bounds a) `inRange` p
                then d
                else a ! p

        newBounds = ((x0-2, y0-2), (x1+2, y1+2)) 
        ((x0,y0), (x1,y1)) = bounds a

enhance' :: Bool -> Enhancer -> Img -> Img
enhance' d e a = array newBounds newImg
    where
        newImg = map newPixel $ range newBounds

        newPixel p = (p, newPixelVal p)

        newPixelVal p = (e Map.!) . bitsToInt . oldBvec $ p

        oldBvec (x,y) =
            [ oldPixel (x-1,y-1), oldPixel (x  ,y-1), oldPixel (x+1,y-1)
            , oldPixel (x-1,y  ), oldPixel (x  ,y  ), oldPixel (x+1,y  )
            , oldPixel (x-1,y+1), oldPixel (x  ,y+1), oldPixel (x+1,y+1)
            ]


        oldPixel p =
            if not $ (bounds a) `inRange` p
                then d
                else a ! p

        newBounds = ((x0-2, y0-2), (x1+2, y1+2)) 
        ((x0,y0), (x1,y1)) = bounds a

popcount :: Img -> Int
popcount = length . filter (== True) . elems

--5687 too high??
--5364 too low -.-

enhance2 e = enhance' True e . enhance' False e

enhanceN e 0 i = i
enhanceN e n i = enhanceN e (traceShowId $ n-2) (enhance2 e i)

main_20 = do
    (enhStr:_:imageLines) <- lines <$> readFile "src/input_20.txt"
    let e = parseEnhancer enhStr
    let img = parseImage (imageLines)

    let img' = enhance False e img
    let img'' = enhance' True e img'

    --printImg img
    putStrLn ""
    --printImg img'
    putStrLn ""
    --printImg img''

    print $ popcount img''

    -- part 2 5726 too low
    -- 5999 too low
    print . popcount . enhanceN e 48 . enhance' True e . enhance False e $ img


