module Day12 where

import qualified Data.Map.Strict as M

type Node = String
type Graph = M.Map Node [Node]

parseGraph :: [String] -> Graph
parseGraph = foldl parseEdge (M.fromList [])
    where
        parseEdge :: Graph -> String -> Graph
        parseEdge g s = addEdge (takeWhile (/= '-') s) (drop 1 . dropWhile (/= '-') $ s) g

        addEdge :: Node -> Node -> Graph -> Graph
        addEdge m n = 
            (M.insertWith ((++)) m [n]) .
            (M.insertWith ((++)) n [m])

main_12_1 = do
    input <- lines <$> readFile "src/input_12_test.txt"
    let g = parseGraph input
    print g