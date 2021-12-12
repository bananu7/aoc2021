module Day12 where

import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.List ((\\))
import Data.Char

type Node = String
type Graph = M.Map Node [Node]
type Path = [Node]

parseGraph :: [String] -> Graph
parseGraph = foldl parseEdge (M.fromList [])
    where
        parseEdge :: Graph -> String -> Graph
        parseEdge g s = addEdge (takeWhile (/= '-') s) (drop 1 . dropWhile (/= '-') $ s) g

        addEdge :: Node -> Node -> Graph -> Graph
        addEdge m n = 
            (M.insertWith ((++)) m [n]) .
            (M.insertWith ((++)) n [m])

neighbors :: Graph -> Node -> [Node]
neighbors g n = g M.! n

neighborsU :: Graph -> [Node] -> Node -> [Node]
neighborsU g visited n = neighbors g n \\ smallVisited
    where
        smallVisited = filter (all (== True) . map isLower) visited

paths :: [Node] -> Graph -> Node -> [Path]
paths visited _ "end" = ["end":visited]
paths visited g n = visit
    where
        visit = concat . map (paths (n : visited) g) $ ns
        ns = neighborsU g visited n


main_12_1 = do
    input <- lines <$> readFile "src/input_12.txt"
    let g = parseGraph input
    --print g

    let ps = paths [] g "start"
    print $ length ps
    --mapM_ print ps

-- -----
{-}
type Visited = M.Map Node Int

neighborsU' :: Graph -> Visited -> Node -> [Node]
neighborsU' g visited n = neighbors g n \\ cantgo
    where
        cantgo = M.keys . M.filterWithKey unable $ visited
        unable "end" = (>= 1)
        unable "start" = (>= 1)
        unable n = if (isUpper . head $ n) then const False else
                    const (any (> 1) . M.elems $ visited)

paths' :: Visited -> Graph -> Node -> [Visited]
paths' visited _ "end" = [M.insertWith (+) "end" 1 visited]
paths' visited g n = visit
    where
        visit = concat . map (paths' (M.insertWith (+) n 1 visited) g) $ ns
        ns = neighborsU' g visited n
-}

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length set
  where set = Set.fromList list

neighborsU' :: Graph -> [Node] -> Node -> [Node]
neighborsU' g visited n = neighbors g n \\ ("start" : cantgo)
    where
        cantgo = if hasDuplicates smallVisited then
            smallVisited
        else
            []

        smallVisited = filter (all (== True) . map isLower) visited


paths' :: [Node] -> Graph -> Node -> [Path]
paths' visited _ "end" = ["end":visited]
paths' visited g n = visit
    where
        visit = concat . map (paths' (n : visited) g) $ ns
        ns = neighborsU' g (n : visited) n

main_12_2 = do
    input <- lines <$> readFile "src/input_12.txt"
    let g = parseGraph input
    --print g

    let ps = paths' [] g "start"
    print $ length ps
    --mapM_ print ps
