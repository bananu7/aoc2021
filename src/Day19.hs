{-# LANGUAGE TupleSections #-}

module Day19 where

import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe

import Debug.Trace

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
    [ [(ss !! ia) `commonDiffs` (ss !! ib) | ib <- [0..length ss -1]] | ia <- [0..length ss -1]]


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

rotsWithNames = zip rots rotNames

neg :: P -> P
neg (x,y,z) = (-x,-y,-z)

findDNorm a b = findDNorm' a b rots rotNames
  where
    findDNorm' _ _ [] _ = Nothing -- should never happen
    findDNorm' a b (f:rs) (n:rns) =
        if f b == a || f (neg b) == a
            then Just (f,n)
            else findDNorm' a b rs rns

findNorm a b =
    case commonDiffs' a b of
        [] -> Nothing
        ds | length ds < 66 -> Nothing
        (d:_) ->
            let
                (((pa1,pa2), da), ((pb1, pb2), db)) = d
                Just (f,n) = findDNorm da db

                delta = 
                        if da == f db then
                            --pa1 `diff` f pb1   -- or pa2-pb2
                            pa2 `diff` f pb2
                        else if da == f (neg db) then
                            pa2 `diff` f pb1   -- or pa2-pb1
                        else
                            error "found norm F doesn't work!"
            in
                Just (f,delta,n)

-- -----------------------------------------

-- can't just do that because some don't have matches
-- norm n = fst $ findNorm 0 n

normAll :: [Scanner] -> Map.Map Int ([P->P], [P], String)
normAll s = normAll' (Map.fromList [ (0, ([rots !! 0], [(0,0,0)], rotNames !! 0)) ]) 0
    where
        normAll' m toI =
            if traceShow toI $ Map.size m == length s then m
            else
                normAll' m' ((toI+1) `mod` length s)
            where
                -- only normalize to "toI" if toI is already normalized
                m' = if toI `Map.member` m 
                        then foldl addNorm m successfulNormsToI
                        else m

                addNorm m (toI, i, (f,d,n)) = 
                    Map.insert i (combine (m Map.! toI) (f,d,n)) m

                combine (fs, ds, ns) (f, d, n2) = (f:fs, d:ds, n2 ++ " -> " ++ ns)

                successfulNormsToI = catMaybes tryNormsToI
                tryNormsToI = map (\i -> (toI,i,) <$> findNorm (s !! toI) (s !! i)) $ (traceShowId allIxesLeft)
                allIxesLeft = filter (/= toI) . filter (\i -> i `Map.notMember` m) $ [0..length s - 1]


{-
norm 0 (x,y,z) = (x,y,z)     -- 0 is assumed norm
norm 1 (x,y,z) = (-z, -x, y)
norm 2 (x,y,z) = norm 3 (z, -x, y) -- go through 3
norm 3 (x,y,z) = (y,x,z)
norm 4 (x,y,z) = norm 3 (-x, -z, -y)
npr
norm 8 (x,y,z) = (-x,z,y)
norm 9 (x,y,z) = (-x, -z, -y)
norm 14 (x,y,z) = (x, -y, z)
norm 16 (x,y,z) = (y,-z,-x)
norm 20 (x,y,z) = (y, z, x)
norm 21 (x,y,z) = (-z, x, y)
norm 27 (x,y,z) = (y, -x, z)
-- undef
norm n (x,y,z) = (x,y,z)
-}

ss = do
    input <- filter (/= "") . lines <$> readFile "src/input_19_test.txt"
    let s = filter (/= []) $ parseScanners input [] []
    return s

applyAll s m =
    map (\i -> apply (m Map.! i) (s !! i)) [0..length s - 1]

add (ax,ay,az) (bx,by,bz) = (ax+bx, ay+by, az+bz)

apply :: ([P->P], [P], String) -> Scanner -> Scanner
apply ([], [], _) s = s
apply ((f:fs), (d:ds), _) s = apply (fs,ds,"") s'
    where
        s' = map ((`add` d) . f) s


-- (438,439,-723)

-- 339 too high
-- 333 too high
main_19 = do
    s <- ss
    let m = normAll s
    print "----"
    print $ Map.size m

    let s' = applyAll s m
    mapM_ (print) $ sort . nub . concat $ s'
    putStrLn $ "final answer: " ++ (show . length . nub . concat $ s')

    --mapM_ (\(i, (f,d,n)) -> putStrLn $ show i ++ " : " ++ n ++ " D :" ++ show d) $ Map.assocs m

-- s0 = +x, +y, +z

-- mapM_ (print . take 3 . sort . map unbased . diffs) $ sd




