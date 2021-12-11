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

neighbors :: Board -> P -> [P]
neighbors b (px,py) = 
    [(px+dx,py+dy) |
     dx <- [-1, 0, 1],
     dy <- [-1, 0, 1],
     (dx,dy) /= (0,0),
     (bounds b) `inRange` (px+dx,py+dy)
     ]

getTens :: Board -> [P]
getTens b = map fst . filter ((== 10) . snd) . assocs $ b

propagate :: P -> State Board ()
propagate p = do
    b <- get
    let ns = map (\p -> (p, b!p + 1)) . neighbors b $ p
    modify $ \b -> b // ns

    let nsFlashing = map fst . filter ((== 10) . snd) $ ns
    mapM_ propagate nsFlashing


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

solve :: Int -> Board -> Int
solve 0 b = 0
solve n b = let (f, b') = step b in f + solve (n-1) b'

main_11_1 = do
    input <- readFile "src/input_11.txt"
    let a = toArr input
    print $ solve 100 a


--

solve' :: Int -> Board -> Int
solve' n b = if f == 100 then n else solve' (n+1) b'
    where (f, b') = step b

main_11_2 = do
    input <- readFile "src/input_11.txt"
    let a = toArr input
    print $ solve' 1 a




