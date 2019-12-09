import Data.List.Split
import Data.Char

part1 :: [[Int]] -> Int
part1 layers = countDigits 2 layer * countDigits 1 layer
    where 
        layer = head $ filter (\l -> countDigits 0 l == min) layers
        min = minimum $ map (countDigits 0) layers
        countDigits digit l = length $ filter (== digit) l

part2 :: [[Int]] -> String
part2  = unlines . chunksOf 25 . map toDigit . foldl1 (zipWith (curry merge))
    where 
        toDigit 1 = '*'
        toDigit _ = ' '
        merge (2, b) = b
        merge (a, _) = a

parseImage :: [Int] -> [[Int]]
parseImage = chunksOf (25 * 6)

main = interact $ part2 . parseImage . map digitToInt . filter isDigit