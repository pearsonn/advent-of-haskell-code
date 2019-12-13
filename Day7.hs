import IntCodes
import Data.List

part1 :: IntCodes -> Integer
part1 codes = maximum $ map runSerial (permutations [0..4])
    where runSerial = foldl' (\output phase -> head (runProgram codes [phase, output])) 0

part2 :: IntCodes -> Integer
part2 codes = maximum $ map (head . input . head . runLoop 0 . initAmplifiers) (permutations [5..9])
    where 
        runLoop :: Integer -> [Program] -> [Program]
        runLoop i amps
            | any terminated amps = amps
            | otherwise = let nextInput = head $ output $ last nextAmps
                          in runLoop nextInput (map (\a -> a { output = [] }) nextAmps)
            where 
                nextAmps = runSerial i amps
                runSerial i amps = scanl feedToNext (next (head amps) i) (tail amps)
                feedToNext a b = if terminated a then a else next b (head $ output a)
        initAmplifiers = map (initProgram codes . pure)

next :: Program -> Integer -> Program
next p i = nextOutput p { input = input p ++ [i] }       

parseInput :: String -> IntCodes
parseInput = read . ('[':) . (++"]")

main = interact $ show . part2 . parseInput