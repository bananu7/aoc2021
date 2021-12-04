module Day4 (main_4_1, main_4_2) where

import Data.List

-- day 4 part 1

type BingoRow = [Int]
-- don't need cols, can keep 10x5 because score will be divided by 2
-- because each number will appear twice
data BingoBoard = BingoBoard [BingoRow] deriving (Show, Eq) --10x5

data CrossResult = Victory Int | StillPlaying BingoBoard deriving (Show, Eq)

cross :: Int -> BingoBoard -> CrossResult
cross x (BingoBoard rows) = checkVictory x (BingoBoard rows')
    where
        rows' = map crossRow rows
        crossRow r = filter (/= x) r

checkVictory :: Int -> BingoBoard -> CrossResult
checkVictory x (BingoBoard rows) = 
    if (length . filter null $ rows) > 0 then
        Victory ((*x) . (`div` 2) . sum . map sum $ rows)
    else
        StillPlaying $ BingoBoard rows

readNumbers :: String -> [Int]
readNumbers = map read . words . map commaToSpace
  where
    commaToSpace ',' = ' '
    commaToSpace x = x

-- needs to be 5 strings
readBoard :: [String] -> BingoBoard
readBoard xs = BingoBoard $ numerical ++ transpose numerical
    where
        numerical = map (map read . words) $ xs

splitInput :: [String] -> [[String]]
splitInput [] = []
splitInput xs = (take 5 . drop 1 $ xs) : (splitInput $ drop 6 xs)

play results (x:numbers) = 
    case findVictory results of
        Just score -> score
        Nothing -> play results' numbers
    where
        results' =  map (cross x . getBoard) $ results

        findVictory :: [CrossResult] -> Maybe Int
        findVictory [] = Nothing
        findVictory (Victory x : _) = Just x
        findVictory (StillPlaying _ : r) = findVictory r 

        getBoard (StillPlaying b) = b

main_4_1 = do
    ls <- lines <$> readFile "src/input_4.txt"
    let numbers = readNumbers $ head ls
    let boards = map readBoard . splitInput $ tail ls

    let score = play (map StillPlaying boards) numbers
    print score

-- day 4 part 2

play2 [lastBoard] (x:numbers) = 
    case lastBoard of
        Victory score -> score
        StillPlaying b -> play [cross x b] numbers

play2 results (x:numbers) = play2 results'' numbers
    where
        results'' = filter isPlaying results'
        isPlaying (StillPlaying _) = True
        isPlaying (Victory _) = False

        results' =  map (cross x . getBoard) $ results

        findVictory :: [CrossResult] -> Maybe Int
        findVictory [] = Nothing
        findVictory (Victory x : _) = Just x
        findVictory (StillPlaying _ : r) = findVictory r 

        getBoard (StillPlaying b) = b

main_4_2 = do
    ls <- lines <$> readFile "src/input_4:.txt"
    let numbers = readNumbers $ head ls
    let boards = map readBoard . splitInput $ tail ls

    let score = play2 (map StillPlaying boards) numbers
    print score
