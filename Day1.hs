import Util

solve1 :: Integral a => [a] -> a
solve1 = sum . map fuel    

solve2 :: Integral a => [a] -> a
solve2 = sum . map totalFuel
                   
totalFuel :: Integral a => a -> a
totalFuel x
    | f <= 0 = 0
    | otherwise = f + totalFuel f
    where f = fuel x

fuel x = x `div` 3 - 2

main = fileInteract "day1.txt" $ show . solve2 . map read . lines