module Day5alt (main_5_1_alt) where

import Day5 (Point, Line(..), parse, gen)

import qualified Data.Map as M

solve :: [Line] -> Int
solve = length . filter (>= 2) . M.elems . foldl draw (M.fromList [])
    where
        draw :: M.Map Point Int -> Line -> M.Map Point Int
        draw m l = M.unionWith (+) m (M.fromList $ zip (gen l) (repeat 1))

main_5_1_alt = do
    input <- lines <$> readFile "src/input_5.txt"
    let ls = map parse input
    let answer = solve ls
    print answer
