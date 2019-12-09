import IntCodes
import Data.List

part1 :: [Int] -> Int
part1 codes = maximum $ map runSerial (permutations [0..4])
    where runSerial = foldl' (\output phase -> head (runProgram codes [phase, output])) 0

part2 :: [Int] -> Int
part2 codes = maximum $ map (head . input . head . runLoop 0 . initAmplifiers) (permutations [5..9])
    where 
        runLoop :: Int -> [Program] -> [Program]
        runLoop i amps
            | any terminated amps = amps
            | otherwise = let nextInput = head $ output $ last nextAmps
                          in runLoop nextInput (map (\a -> a { output = [] }) nextAmps)
            where 
                nextAmps = runSerial i amps
                runSerial i amps = scanl feedToNext (nextOutput (head amps) i) (tail amps)
                feedToNext a b = if terminated a then a else nextOutput b (head $ output a)
        initAmplifiers = map (initProgram codes . (:[]))

--run the program with the given input until it generates an output or terminates
nextOutput :: Program -> Int -> Program
nextOutput p i = next p { input = input p ++ [i] }
        where 
            next p@(Program { terminated = True }) = p { output = [i] }
            next p@(Program { output = (x:[])}) = p
            next p = next $ execute p $ nextInstruction p --recursively execute next instruction        

parseInput :: String -> IntCodes
parseInput = read . ('[':) . (++"]")

main = interact $ show . part2 . parseInput