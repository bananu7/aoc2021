module Day18 where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import Data.Void
import Data.Maybe (fromJust)

type Parser = Parsec Void String
data Tree = L Int | N Tree Tree deriving (Eq)

instance Show Tree where
    show (L x) = show x
    show (N a b) = '[' : show a ++ "," ++ show b ++ "]"

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme (return ())

int :: Parser Int
int = lexeme Lex.decimal

tree :: Parser Tree
tree = do
    _ <- char '['
    a <- try $ (L <$> int) <|> tree
    _ <- char ','
    b <- try $ (L <$> int) <|> tree
    _ <- char ']'
    return $ N a b

parseTree :: String -> Maybe Tree
parseTree s = 
    case runParser tree "" s of
        Right t -> Just t
        Left _ -> Nothing

reduce :: Tree -> Tree
reduce t = 
    case explode t of
        Right t' -> reduce t'
        Left t ->
            case split t of
                Right t' -> reduce t'
                Left t -> t

-- If any pair is nested inside four pairs, the leftmost such pair explodes.
-- To explode a pair, the pair's left value is added to the first regular
-- number to the left of the exploding pair (if any), and the pair's right
-- value is added to the first regular number to the right of the exploding pair (if any).
-- Exploding pairs will always consist of two regular numbers.
explode :: Tree -> Either Tree Tree
explode t = 
    case explode' 0 t of
        Left t -> Left t
        Right (_, t, _) -> Right t
    where
        explode' :: Int -> Tree -> Either Tree (Maybe Int, Tree, Maybe Int)
        explode' _ (L x) = Left $ L x

        explode' n (N (L a) (L b)) | n >= 4 = 
            Right (Just a, L 0, Just b)

        explode' n (N a b) =
            case explode' (n+1) a of
                Right (lv, a', Just rv) -> 
                    case addR rv b of
                        Right b' -> Right (lv, N a' b', Nothing)
                        Left b -> Right (lv, N a' b, Just rv)

                Right (lv, a', Nothing) -> Right (lv, N a' b, Nothing)

                Left a -> case explode' (n+1) b of
                    Right (Just lv, b', rv) ->
                        case addL lv a of
                            Right a' -> Right (Nothing, N a' b', rv)
                            Left a -> Right (Just lv, N a b', rv)
                    Right (Nothing, b', rv) -> Right (Nothing, N a b', rv)
                    Left b -> Left $ N a b


addL :: Int -> Tree -> Either Tree Tree
addL v (L x) = Right (L $ x + v)
addL v (N a b) =
    case addL v b of
        Right b' -> Right $ N a b'
        Left b -> case addL v a of
                    Right a' -> Right $ N a' b
                    Left a -> Left $ N a b

addR :: Int -> Tree -> Either Tree Tree
addR v (L x) = Right (L $ x + v)
addR v (N a b) =
    case addR v a of
        Right a' -> Right $ N a' b
        Left a -> case addR v b of
                    Right b' -> Right $ N a b'
                    Left b -> Left $ N a b

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















