
import Data.List.Split
import Data.List
import qualified Data.Set as Set
import Util

type Point = (Int, Int)

input = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

start :: Point
start = (0, 0)
   
solve1 :: [[String]] -> Int
solve1 (xs:ys:[]) = minimum $ map manhatten $ intersections (toPoints start xs) (toPoints start ys)
    where        
        manhatten p2 = abs (fst start - fst p2) + abs (snd start - snd p2)        

solve2 :: [[String]] -> Int
solve2 (xs:ys:[]) = minimum $ map steps i
    where
        steps p = length (takeWhile (/=p) px) + length (takeWhile (/=p) py) + 2
        i = intersections px py
        px = toPoints start xs
        py = toPoints start ys

intersections :: [Point] -> [Point] -> [Point]
intersections p1 p2 = filter (/=start) $ Set.toList $ Set.intersection (Set.fromList p1) (Set.fromList p2)       
        
toPoints :: Point -> [String] -> [Point]
toPoints _ [] = []
toPoints s@(x, y) ((dir:distance):ds) = tail [(a, b) | a <- range x x', b <- range y y'] ++ toPoints p ds
    where        
        p@(x', y') = case dir of
            'U' -> (x, y + distance')
            'D' -> (x, y - distance')
            'R' -> (x + distance', y)
            'L' -> (x - distance', y)
        distance' = read distance
        range a b
            | a < b = [a..b]
            | otherwise = [a, a-1..b]

main = fileInteract "day3.txt" $ show . solve2 . map (splitOn ",") . lines