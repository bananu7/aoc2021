module Day18alt where

import Day18 (Parser, Tree(..), parseTree)

import Data.Maybe (fromJust)

eithermap :: (Tree -> Tree) -> (Tree -> Tree) -> Either Tree Tree -> Tree
eithermap f _ (Left t) = f t
eithermap _ g (Right t) = g t

reduce :: Tree -> Tree
reduce = eithermap (eithermap id reduce . split) reduce . explode
{-
    case explode t of
        Right t' -> reduce t'
        Left t ->
            case split t of
                Right t' -> reduce t'
                Left t -> t
-}

-- If any pair is nested inside four pairs, the leftmost such pair explodes.
-- To explode a pair, the pair's left value is added to the first regular
-- number to the left of the exploding pair (if any), and the pair's right
-- value is added to the first regular number to the right of the exploding pair (if any).
-- Exploding pairs will always consist of two regular numbers.
explode :: Tree -> Either Tree Tree
explode t = 
    case explode' 0 t of
        Nothing -> Left t
        Just (_, t, _) -> Right t
    where
        explode' :: Int -> Tree -> Maybe (Maybe Int, Tree, Maybe Int)
        explode' _ (L x) = Nothing

        explode' n (N (L a) (L b)) | n >= 4 = 
            Just (Just a, L 0, Just b)

        explode' n (N a b) =
            case explode' (n+1) a of
                Just (lv, a', Just rv) -> Just (lv, N a' (addR rv b), Nothing)
                Just (lv, a', Nothing) -> Just (lv, N a' b, Nothing)

                Nothing -> case explode' (n+1) b of
                    Just (Just lv, b', rv) -> Just (Nothing, N (addL lv a) b', rv)
                    Just (Nothing, b', rv) -> Just (Nothing, N a b', rv)
                    Nothing -> Nothing


addL :: Int -> Tree -> Tree
addL v (L x) = L $ x + v
addL v (N a b) = N a (addL v b)

addR :: Int -> Tree -> Tree
addR v (L x) = L $ x + v
addR v (N a b) = N (addR v a) b

-- If any regular number is 10 or greater, the leftmost such regular number splits.
split :: Tree -> Either Tree Tree
split (L x) = if x >= 10 then Right (N (L a) (L b)) else Left (L x)
    where
        a = x `div` 2
        b = a + x `rem` 2

split (N a b) =
    case split a of
        Right a' -> Right $ N a' b -- mod happened, backtrack
        Left a -> case (split b) of
                    Right b' -> Right $ N a b'
                    Left b -> Left $ N a b -- no mod in either branch


add :: Tree -> Tree -> Tree
add a b = reduce $ N a b

pr = fromJust.parseTree
{-}
l = pr "[[[[4,3],4],4],[7,[[8,4],9]]]"
r = pr "[1,1]"
a = l `add` r-}

magnitude :: Tree -> Int
magnitude (L x) = x
magnitude (N a b) = 3*magnitude a + 2*magnitude b

main_18_1 = do
    input <- lines <$> readFile "src/input_18.txt"
    let ts = map pr input

    let v = foldl1 add ts
    print v
    print $ magnitude v


-- part2

addMax :: Tree -> Tree -> Int
addMax x y = max (magnitude $ x `add` y) (magnitude $ y `add` x)

max2 (x:y:[]) = addMax x y
max2 (x:xs) = max withX (max2 xs)
    where
        withX = maximum . map (addMax x) $ xs


main_18_2 = do
    input <- lines <$> readFile "src/input_18.txt"
    let ts = map pr input
    print $ max2 ts















