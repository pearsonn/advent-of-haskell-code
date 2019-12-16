
import qualified Data.Matrix as M
import Data.Ratio
import Data.List (nub)

testInput = "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"
testInput2 = "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###."
testInput3 = ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#.."

data Heading = Positive | Negative
data Slope = Infinity | Slope (Ratio Int)
type Asteroid = (Heading, Slope)

part1 :: M.Matrix Char -> (Int, Int)
part1 m = head $ filter (\p -> (length $ slopes p) == max) asteroidPs
    where 
        max =  maximum $ map (length . slopes) asteroidPs
        slopes :: (Int, Int) -> [Double]
        slopes p = slopes' posX ++ slopes' negX
            where 
                (posX, negX) = break (\(x, y) -> x > fst p) $ filter (/=p) asteroidPs 
                slopes' = nub . map (slope p)
        asteroidPs = [(x, y) | x <- [1..M.ncols m], y <- [1..M.nrows m], M.getElem x y m == '#']

station = (30,27)

part2 :: M.Matrix Char -> Int
part2 m = undefined
    where
        destroy posX negX  = undefined        
        asteroidPs = [(x, y) | x <- [1..M.ncols m], y <- [1..M.nrows m], M.getElem x y m == '#']

slope (x1, y1) (x2, y2) 
    | y1 - y2 == 0 = if x1 - x2 < 0 then -1e10 else 1e10
    | x1 - x2 == 0 = if y1 - y2 < 0 then -1e-10 else 1e-10
    | otherwise = (fromIntegral x1 - fromIntegral x2) / (fromIntegral y1 - fromIntegral y2)

main = interact $ show . part1 . M.fromLists . lines