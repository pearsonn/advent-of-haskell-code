import Data.List

start = 134792
end = 675810

isIncreasing :: String -> Bool
isIncreasing xs = and $ zipWith (<=) ('0':xs) xs

containsPair :: String -> Bool
containsPair xs = length (group xs) /= length xs 

containsPairExact :: String -> Bool
containsPairExact = any (==2) . map length . group

solve :: Int
solve = length $ filter (\x -> isIncreasing x && containsPair x) $ map show [start..end]

solve2 :: Int
solve2 = length $ filter (\x -> isIncreasing x && containsPairExact x) $ map show [start..end]