module Day19 where

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

main_19 = do
    input <- filter (/= "") . lines <$> readFile "src/input_19.txt"
    let s = filter (/= []) $ parseScanners input [] []

    print $ length s
    print $ s !! 0
    print $ last s





