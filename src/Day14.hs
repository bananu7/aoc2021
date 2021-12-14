module Day14 where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Data.Char
import Data.Ord

type Mapping = Map.Map (Char,Char) Char

parseMappings :: [String] -> Mapping
parseMappings = Map.fromList . map parseMapping
    where
        parseMapping (a:b:' ':'-':'>':' ':r:_) = ((a,b), r)

type Elem = (Char, Char)


type Polymer = (Map.Map (Char, Char) Int, Map.Map Char Int)

mapCount xs = foldl (\m e -> Map.insertWith (+) e 1 m) mempty xs
mapCountN xs = foldl (\m (e,n) -> Map.insertWith (+) e n m) mempty xs

strToPoly :: String -> Polymer
strToPoly s = (pa,pb)
    where
        pa = Map.fromList $ zip (zip s (drop 1 s)) (repeat 1)
        pb = mapCount s

polymerize :: Mapping -> Polymer -> Polymer
polymerize m (pa, pb) = foldl f (mempty, pb) results
    where
        f (pa, pb) (Left (e, n)) = 
            (Map.insertWith (+) e n pa, pb) -- no change to count

        f (pa,pb) (Right ((e1, e2), n)) =
            (Map.insertWith (+) e1 n . Map.insertWith (+) e2 n $ pa,
             Map.insertWith (+) (snd e1) n $ pb)

        results = map (polymerizeOne m) . Map.assocs $ pa

polymerizeOne :: Mapping -> (Elem, Int) -> Either (Elem, Int) ((Elem, Elem), Int) -- either the same or two new pairs
polymerizeOne m ((a,b), n) = 
    case Map.lookup (a,b) m of
        Just c -> Right (((a,c), (c,b)), n)
        Nothing -> Left ((a,b), n)

score :: Polymer -> Int
score (_, pb) = (snd.last $ scounts) - (snd . head $ scounts)
    where
        scounts = sortBy (comparing snd) . Map.assocs $ pb

-- part1

{-}
type Polymer = String

strToPoly :: String -> String
strToPoly = id

polymerize :: Mapping -> Polymer -> Polymer
polymerize m (a:[]) = [a]
polymerize m (a:b:rest) = polymerizeOne m (a,b) ++ polymerize m (b:rest)

polymerizeOne :: Mapping -> Elem -> [Char] -- either the same or two new pairs
polymerizeOne m (a,b) = 
    case Map.lookup (a,b) m of
        Just c -> [a,c]
        Nothing -> [a]

score :: Polymer -> Int
score p = (snd.last $ scounts) - (snd . head $ scounts)
  where
    scounts = sortBy (comparing snd) . Map.assocs $ counts
    counts = foldl (\m c -> Map.insertWith (+) c 1 m) mempty p
-}

pr p = do
    print . Map.assocs . fst $ p
    print . Map.assocs . snd $ p
    print . sum . Map.elems . snd $ p
    print "-----"

main_14_1 = do
    (startS:"":mLines) <- lines <$> readFile "src/input_14.txt"
    let m = parseMappings mLines
    let start = strToPoly startS

    pr start

    let polys = iterate (polymerize m) start

    --pr . head . drop 1 $ polys
    --pr . head . drop 2 $ polys
    --pr . head . drop 3 $ polys
    --pr . head . drop 10 $ polys
    print . score . head . drop 40 $ polys
{-}
    print . length . head . drop 5 $ polys
    print . length . head . drop 10 $ polys
    print . score . head . drop 10 $ polys


    print "-----"
    print . length . head . drop 30 $ polys-}

    return ()