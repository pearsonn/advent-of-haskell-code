import IntCodes

part1 :: IntCodes -> [Integer]
part1 = flip runProgram [1]

part2 :: IntCodes -> [Integer]
part2 = flip runProgram [2]

parseInput :: String -> IntCodes
parseInput = read . ('[':) . (++"]")

main = interact $ show . part2 . parseInput