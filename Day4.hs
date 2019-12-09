import Data.List
import Control.Monad

start = 134792
end = 675810

isIncreasing :: Ord a => [a] -> Bool
isIncreasing xs = and $ zipWith (<=) xs (tail xs)

containsPair :: Ord a => [a] -> Bool
containsPair xs = length (group xs) /= length xs 

containsPairExact :: Ord a => [a] -> Bool
containsPairExact = any (==2) . map length . group

solve :: Int
solve = length $ filter (liftM2 (&&) isIncreasing containsPair) $ map show [start..end]

solve2 :: Int
solve2 = length $ filter (liftM2 (&&) isIncreasing containsPairExact) $ map show [start..end]