module Day19 where

import Data.List
import qualified Data.Set as Set
import Data.Maybe

type P = (Int,Int,Int)
type Scanner = [P]


parseScanners :: [String] -> [Scanner] -> [P] -> [Scanner]
parseScanners [] scanners ps = reverse $ (reverse ps) : scanners
parseScanners (l:s) scanners ps =
    case l of
        ('-':'-':'-':_) -> parseScanners s (reverse ps:scanners) []
        "" -> parseScanners s scanners ps
        _ -> parseScanners s scanners (parsePoint l : ps)

parsePoint :: String -> P
parsePoint s = (read xs, read ys, read zs)
    where 
        [xs,ys,zs] = words . map rep $ s
        rep ',' = ' '
        rep a = a

-- -----------------------------------------

diff :: P -> P -> P
diff (ax,ay,az) (bx,by,bz) = (ax-bx, ay-by, az-bz)

-- TODO, 48 instead of 24 transformations
-- can't be just abs and sort, must respect coordinate rotation
-- use for diffs
unbased (ax,ay,az) = (sort . map abs $ [ax,ay,az])
-- use for diffs
diffComp a b = unbased a == unbased b

diffs s = [a `diff` b | a <- s, b <- s, a /= b, a > b]

commonDiffs sa sb = Set.intersection dfa dfb
    where
        dfa = Set.fromList . map unbased . diffs $ sa
        dfb = Set.fromList . map unbased . diffs $ sb

{-}
commonDiffs' sa sb = catMaybes . map (combine dfb) $ dfa
    where
        combine :: [P] -> P -> Maybe (P, P)
        combine [] da = Nothing
        combine (db:ds) da =
            if unbased da == unbased db
                then Just (da,db)
                else combine ds da

        dfa = diffs $ sa
        dfb = diffs $ sb
-}
diffs' s = [((a,b), a `diff` b) | a <- s, b <- s, a /= b, a > b]

commonDiffs' sa sb = catMaybes . map (combine dfb) $ dfa
    where
        --combine :: [P] -> P -> Maybe (P, P)
        combine [] _ = Nothing
        combine ((pb, db):ds) (pa, da) =
            if unbased da == unbased db
                then Just ((pa,da),(pb,db))
                else combine ds (pa, da)

        dfa = diffs' $ sa
        dfb = diffs' $ sb

allCommonDiffs ss =
    [ [(ss !! ia) `commonDiffs` (ss !! ib) | ib <- [0..length ss -1], ia < ib] | ia <- [0..length ss -1]]

-- n(n-1)/2 = Gk
-- n(n-1) = Gk * 2
-- n^2 - n = Gk * 2
numDiffsToNumPoints 0 = 0
numDiffsToNumPoints 1 = 2
numDiffsToNumPoints 3 = 3
numDiffsToNumPoints 6 = 4
numDiffsToNumPoints 10 = 5
numDiffsToNumPoints 15 = 6
numDiffsToNumPoints 21 = 7
numDiffsToNumPoints 28 = 8
numDiffsToNumPoints 66 = 12
numDiffsToNumPoints 325 = 26


rots :: [P -> P]
rots = [
    -- rotate around Z, z+ towardz Z+
     (\(x,y,z) -> (x,y,z))
    ,(\(x,y,z) -> (y,-x,z))
    ,(\(x,y,z) -> (-x,-y,z))
    ,(\(x,y,z) -> (-y,x,z))

    -- rotate around Z, z- towards Z+
    ,(\(x,y,z) -> (-x,y,-z))
    ,(\(x,y,z) -> (y,x,-z))
    ,(\(x,y,z) -> (x,-y,-z))
    ,(\(x,y,z) -> (-y,-x,-z))

    -- rotate around Y, y+ towards Z+
    ,(\(x,y,z) -> (z, x, y))
    ,(\(x,y,z) -> (x,-z, y))
    ,(\(x,y,z) -> (-z,-x, y))
    ,(\(x,y,z) -> (-x,z, y))

    -- rotate around Y, y+ towards Z+
    ,(\(x,y,z) -> (-z, x, -y))
    ,(\(x,y,z) -> (x, z, -y))
    ,(\(x,y,z) -> (z, -x, -y))
    ,(\(x,y,z) -> (-x,-z, -y))

    -- rotate around X, x+ towards Z+
    ,(\(x,y,z) -> (y, z, x))
    ,(\(x,y,z) -> (z,-y, x))
    ,(\(x,y,z) -> (-y,-z, x))
    ,(\(x,y,z) -> (-z,y, x))

    -- rotate around X, x- towards Z+
    ,(\(x,y,z) -> (-y, z, -x))
    ,(\(x,y,z) -> (z, y, -x))
    ,(\(x,y,z) -> (y, -z, -x))
    ,(\(x,y,z) -> (-z,-y, -x))
    ]

rotNames = [
     "(x,y,z)"
    ,"(y,-x,z)"
    ,"(-x,-y,z)"
    ,"(-y,x,z)"
    ,"(-x,y,-z)"
    ,"(y,x,-z)"
    ,"(x,-y,-z)"
    ,"(-y,-x,-z)"
    ,"(z, x, y)"
    ,"(x,-z, y)"
    ,"(-z,-x, y)"
    ,"(-x,z, y)"
    ,"(-z, x, -y)"
    ,"(x, z, -y)"
    ,"(z, -x, -y)"
    ,"(-x,-z, -y)"
    ,"(y, z, x)"
    ,"(z,-y, x)"
    ,"(-y,-z, x)"
    ,"(-z,y, x)"
    ,"(-y, z, -x)"
    ,"(z, y, -x)"
    ,"(y, -z, -x)"
    ,"(-z,-y, -x)"
    ]

neg :: P -> P
neg (x,y,z) = (-x,-y,-z)

findDNorm a b = findDNorm' a b rots rotNames
  where
    findDNorm' _ _ [] _ = error "no matching rot"
    findDNorm' a b (f:rs) (n:rns) =
        if f b == a || f (neg b) == a then n
            else findDNorm' a b rs rns

findNorm a b = findDNorm da db
    where
        (((pa1,pa2), da), ((pb1, pb2), db)) = d
        d = head $ commonDiffs' a b

-- -----------------------------------------

norm 0 (x,y,z) = (x,y,z)     -- 0 is assumed norm
norm 1 (x,y,z) = (-z, -x, y)
norm 3 (x,y,z) = (y,x,z)
-- undef
norm n (x,y,z) = (x,y,z)

normN scanners = map (\(s,i) -> map (norm i) s) . zip scanners $ [0..]

ss = do
    input <- filter (/= "") . lines <$> readFile "src/input_19.txt"
    let s = filter (/= []) $ parseScanners input [] []
    return s

main_19 = do
    s <- ss

    print $ length (s !! 0)
    print . length . diffs $ (s !! 0)

    print $ allCommonDiffs s

-- s0 = +x, +y, +z

-- mapM_ (print . take 3 . sort . map unbased . diffs) $ sd




