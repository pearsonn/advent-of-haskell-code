import IntCodes

part1 :: [Int] -> [Int]
part1 = getDiagnosticCode [1]

part2 :: [Int] -> [Int]
part2 = getDiagnosticCode [5]

getDiagnosticCode :: [Int] -> ([Int] -> [Int])
getDiagnosticCode = flip runProgram

main = interact $ show . part2 . read . ('[':) . (++"]")