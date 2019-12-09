import IntCodes

part1 :: IntCodes -> [Integer]
part1 = getDiagnosticCode [1]

part2 :: IntCodes -> [Integer]
part2 = getDiagnosticCode [5]

getDiagnosticCode :: [Integer] -> IntCodes -> [Integer]
getDiagnosticCode = flip runProgram

main = interact $ show . part2 . read . ('[':) . (++"]")