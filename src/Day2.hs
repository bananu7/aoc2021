module Day2 (main_3, main_4) where

-- day 2 part 1
data Move = Up Int | Down Int | Forward Int

eval :: [Move] -> Int
eval = (\(x,y)->x*y) . foldl evalOne (0,0)
    where
        evalOne (d, f) (Up x) = (d-x, f)
        evalOne (d, f) (Down x) = (d+x, f)
        evalOne (d, f) (Forward x) = (d, f+x)

parseLine :: String -> Move
parseLine line =
    case words line of
        [moveS, vS] -> parseMove moveS $ read vS
        _ -> error "wrong line format"
    where
        parseMove "up" = Up
        parseMove "down" = Down
        parseMove "forward" = Forward

main_3 =
    readFile "src/input_2.txt" >>=
    return . eval . map parseLine . lines >>=
    print


-- day 2 part 3

evalAim :: [Move] -> Int
evalAim = (\(p,d,a) -> p*d) . foldl evalOne (0,0,0)
    where
        evalOne (p, d, a) (Up x)       = (p, d, a-x)
        evalOne (p, d, a) (Down x)     = (p, d, a+x)
        evalOne (p, d, a) (Forward x)  = (p+x, d+x*a, a)

main_4 =
    readFile "src/input_2.txt" >>=
    return . evalAim . map parseLine . lines >>=
    print
