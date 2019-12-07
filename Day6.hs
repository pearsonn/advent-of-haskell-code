import qualified Data.Tree as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

type AdjacencyList =  [(String, String)]

--Takes an adjacency list and return the number of orbits
part1 :: AdjacencyList -> Int
part1 = sum . map (\(l, n) -> l * length n) . zip [0..] . T.levels . asTree "COM"

asTree :: String -> AdjacencyList -> T.Tree String
asTree root edges = T.unfoldTree getNode root
    where    
        getNode val = (val, fromMaybe [] (M.lookup val adjMap))        
        adjMap = M.fromListWith (++) $ map (\(a, b) -> (a, [b])) edges

part2 :: AdjacencyList -> Int
part2 = pathLength "YOU" "SAN"
        
--only works for two leaf nodes
pathLength :: String -> String -> AdjacencyList -> Int
pathLength from to edges = length (takeWhile (/=a) (parents from edges)) + length (takeWhile (/=a) (parents to edges))
    where a = ancestor from to edges

ancestor :: [Char] -> String -> AdjacencyList -> String
ancestor a b edges = head $ filter (flip S.member parentsTo) (parents b edges)   
    where parentsTo = S.fromList $ parents a edges

parents :: String -> AdjacencyList -> [String]        
parents node edges = parents' node
    where 
        parents' n = case M.lookup n parentsMap of
            Nothing -> []
            Just val -> val : parents' val
        parentsMap = M.fromList (map swap edges)

parseInput :: String -> AdjacencyList
parseInput = map (\o -> (take 3 o, drop 4 o)) . lines

main = interact $ show . part2 . parseInput