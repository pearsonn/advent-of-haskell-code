import IntCodes
import qualified Data.Map.Strict as Map

type Screen = Map.Map (Int, Int) Int

main :: IO ()
main = interact $ show . part1 . parseInput

parseInput :: String -> IntCodes
parseInput = read . ('[':) . (++"]")

screenToString :: Screen -> String
screenToString = undefined

part1 :: IntCodes -> Int
part1 = length . filter (==2) . map snd . Map.toList . draw Map.empty . map fromInteger . reverse . flip runProgram []
    where     
        draw s [] = s
        draw s (x:y:c:xs) = draw (Map.insert (x, y) c s) xs

part2 :: IntCodes -> Int
part2 = undefined