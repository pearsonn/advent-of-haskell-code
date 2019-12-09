import IntCodes
import Data.List

part1 :: [Int] -> Int
part1 codes = maximum $ map runSerial (permutations [0..4])
    where runSerial = foldl' (\output phase -> head (runProgram codes [phase, output])) 0

part2 :: [Int] -> Int
part2 codes = maximum $ map (head . output . last . flip runLoop 0 . amplifiers) (permutations [5..9])
    where 
        runLoop :: [Program] -> Int -> [Program]
        runLoop as i = undefined
            where feedToNext a b = if terminated a then a else nextOutput b (head $ output a)
        amplifiers :: [Int] -> [Program]
        amplifiers = map (initProgram codes . (:[]))

--run the program with the given input until it generates an output or terminates
nextOutput :: Program -> Int -> Program
nextOutput p i = next p { input = [i] }
        where 
            next p@(Program { terminated = True }) = p { output = input p }
            next p@(Program { output = (x:[])}) = p
            next p = next $ execute p $ nextInstruction p --recursively execute next instruction        

main = interact $ show . part2 . read . ('[':) . (++"]")