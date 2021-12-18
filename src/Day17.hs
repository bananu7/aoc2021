module Day17 where

import Debug.Trace

-- target area: x=201..230, y=-99..-65

--1176 wrong


xmin = 201
xmax = 230
ymin = -99
ymax = -65

{-
xmin = 20
xmax = 30
ymin = -10
ymax = -5
-}

type P = (Int, Int)

ok :: P -> Bool
ok (x,y) = x>=xmin && x<=xmax && y>=ymin && y <= ymax

-- Just y if succeeds
sim :: P -> P -> [P] -> Either [P] [P]
sim (vx, vy) (x,y) path =
    if y < ymin then Left path'
    else if ok (x,y) then Right path'
    else sim (vx', vy') (x+vx, y+vy) path'
    where
        vx' = max (vx-1) 0
        vy' = vy - 1
        path' = ((x,y):path)

shoot v = sim v (0,0) []

score :: [P] -> Int
score p = maximum . map snd $ p


simY :: Int -> Int -> [Int] -> Either [Int] [Int]
simY vy y p =
    if y < ymin then Left p'
    else if y>=ymin && y <= ymax then Right p'
    else simY vy' y' p'
    where
        vy' = vy-1
        y' = y + vy
        p' = y:p

optY :: Int -> (Int, Int)
optY vy = 
    case simY vy 0 [] of
        Left p -> (vy,0)
        Right p -> choose (vy, maximum p) (optY (vy+1))
    where
        choose (v1, y1) (v2, y2) = if y2 > y1 then (v2,y2) else (v1,y1)



optimize :: P -> Int -> Int
optimize (0,_) best = best
optimize (20, _) best = best 
optimize (vx,vy) best =
    case shoot (vx,vy) of
        Left p -> maximum [
                            optimize (vx+1, vy) best,
                            optimize (vx+1, vy-1) best
                            ]
        Right p -> traceShow (vx,vy) $ score p


-- part 2

-- vy max = 0, 1, ... (-ymin - 1)
-- one step basically
-- vy min = 0, -1, -2 .. ymin 

isRight (Right _) = True
isRight (Left _) = False

-- 1039 too low

part2 = length [(vx, vy) | vx <- [1.. 230], vy <- [ymin .. (-ymin)-1], isRight (shoot (vx, vy)) ]

