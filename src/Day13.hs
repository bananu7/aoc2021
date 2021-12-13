module Day13 where

import qualified Data.Set as Set

type Point = (Int, Int)
type Board = Set.Set Point

data Fold = FoldX Int | FoldY Int deriving (Show, Eq)

parsePoint :: String -> Point
parsePoint s = (read . takeWhile (/=',') $ s, read . drop 1 . dropWhile (/=',') $ s)

parseFold :: String -> Fold
parseFold s = pf $ drop 11 s
  where
    pf ('x':'=':n) = FoldX (read n)
    pf ('y':'=':n) = FoldY (read n)

parse :: [String] -> (Board, [Fold])
parse xs = (Set.fromList points, folds)
  where
    points = map parsePoint pointLines
    pointLines = takeWhile (/="") xs

    folds = map parseFold foldLines
    foldLines = drop 1. dropWhile (/="") $ xs

reflect a f = f - (a-f) -- a > f
{-
8 5 -> 2*5-8 = 10 - 8 = 2
....|..x.
.x..|....
-}

fold :: Fold -> Board -> Board
fold (FoldX fx) b = (b `Set.difference` n) `Set.union` n'
    where
        n = Set.filter (\(x,_) -> x > fx) b
        n' = Set.map (\(x,y) -> (reflect x fx, y)) n

fold (FoldY fy) b = (b `Set.difference` n) `Set.union` n'
    where
        n = Set.filter (\(_, y) -> y > fy) b
        n' = Set.map (\(x,y) -> (x, reflect y fy)) n


-- -----


pr :: Board -> Int -> IO ()
pr b y = print s
    where
        s = map (v . (`Set.member` b)) $ [(x,y) | x <- [0..50]] 
        v True = '#'
        v False = '.'

main_13 = do
    input <- lines <$> readFile "src/input_13.txt"
    let (b,fs) = parse input
    let b' = fold (head fs) b
    print $ Set.size b'

    let bf = foldl (flip fold) b fs
    print $ Set.size bf


    mapM_ (pr bf) [0..8]





