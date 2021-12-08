module Day8 where

import qualified Data.Map as M

type RandomDg = String

parse :: String -> [RandomDg]
parse s = words . drop 1 . dropWhile (/= '|') $ s

dig1478 :: RandomDg -> Bool
dig1478 xs =
    case length xs of
        2 -> True
        3 -> True
        4 -> True
        7 -> True
        _ -> False

main_8_1 = do
    input <- readFile "src/input_8.txt"
    let digits = concat . map parse . lines $ input

    let answer = length . filter (dig1478) $ digits
    print answer

-- be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe

-- 0: abc.efg  | 6
-- 1: ..c..f.  | 2
-- 2: a.cde.g  | 5
-- 3: a.cd.fg  | 5
-- 4: .bcd.f.  | 4
-- 5: ab.d.fg  | 5
-- 6: ab.defg  | 6
-- 7: a.c..f.  | 3
-- 8: abcdefg  | 7
-- 9: abcd.fg  | 6


-- 0: 6, missing d
-- 1: len
-- 2: 5, contains acde
-- 3: 5, missing e but has acd
-- 4: len
-- 5: 5, missing c
-- 6: 6, missing c
-- 7: len
-- 8: len
-- 9: 6, missing e

-- need to find: acde

-- find 2-element string, they are possible c,f
-- find 3-element string, remove cf = known a
-- find 4-element string, they are possible b,d

-- 6 element with two new but missing c/f is '6', and c is missing
-- 6 element with two new but issing b/d/ is '0', and d is missing
-- 6 elemen with bcdf - it's 9, e missing

type Line = ([RandomDg],[RandomDg])
type Dt = [Line]

type CF = (Char, Char)
type BD = (Char, Char)

findCF :: Line -> CF -- candidates
findCF (i,o) = (c, f)
    where
        (c:f:[]) = head . filter (\d -> length d == 2) $ i

determineA :: Line -> CF -> Char
determineA (i, o) (c, f) = a
    where
        a = head . filter (\x -> x /= c && x /= f) . head . filter (\d -> length d == 3) $ i

findBD :: Line -> CF -> BD -- candidates
findBD (i,o) (c, f) = (b,d)
    where
        (b:d:[]) = filter (\x -> x /= c && x /= f) . head . filter (\d -> length d == 4) $ i

determineE :: Line -> CF -> BD -> Char
determineE (i, o) (c,f) (b,d) = e
    where
        e = head . missingIn $ nine
        nine = head . filter isNum9 $ i
        isNum9 n = length n == 6 && containsAll [b,c,d,f] n


determineC :: Line -> CF -> BD -> Char
determineC (i, o) (c,f) (b,d) = c'
    where
        c' = head . missingIn $ s
        s = head . filter (containsOneOf (c,f)) $ all6WithBD
        all6WithBD = filter (containsAll [b,d]) $  all6
        all6 = filter (\d -> length d == 6) $ i


determineD :: Line -> CF -> BD -> Char
determineD (i, o) (c,f) (b,d) = d'
    where
        d' = head . missingIn $ zero
        zero = head . filter (containsOneOf (b,d)) $ all6WithCF
        all6WithCF = filter (containsAll [c,f]) $  all6
        all6 = filter (\d -> length d == 6) $ i

type ACDE = (Char, Char, Char, Char)

-- BCDEF
detLine :: Line -> ACDE
detLine x =
    let
        cf = findCF x
        bd = findBD x cf
        a = determineA x cf
        e = determineE x cf bd
        c = determineC x cf bd
        d = determineD x cf bd
    in
        (a,c,d,e)

detDigit :: ACDE -> String  -> Int
detDigit (a,c,d,e) s =
    case length s of
        2 -> 1
        3 -> 7
        4 -> 4
        5 -> detDigit5
        6 -> detDigit6
        7 -> 8
    where
        detDigit5 =
            if containsAll [a,c,d] s && e `elem` missingIn s then
                3
            else if c `elem` missingIn s && e `elem` missingIn s then
                5
            else
                2

        detDigit6 =
            case missingIn s of
                [m] | m == c -> 6
                [m] | m == d -> 0
                [m] | m == e -> 9


missingIn n = filter (`notElem` n) ['a'..'g']
containsAll qs n = all (==True) . map (`elem` n) $ qs

containsOneOf (a,b) n = (a `elem` n) /= (b `elem` n)


parse2 :: String -> Line
parse2 s = (i,o)
    where
        i = words . takeWhile (/= '|') $ s
        o = words . drop 1 . dropWhile (/= '|') $ s

--xx = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
xx = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
x = parse2 xx
cf = findCF x
bd = findBD x cf

solve :: Line -> Int
solve x = 
    let
        acde = detLine x
        [d0, d1, d2, d3] = map (detDigit acde) $ snd x
    in
        d0*1000 + d1*100 + d2*10 + d3

main_8_2 = do
    input <- readFile "src/input_8.txt"
    let cases = map parse2 . lines $ input

    --print $ map (filter (==2) . map length . fst) cases
    print $ sum . map solve $ cases



