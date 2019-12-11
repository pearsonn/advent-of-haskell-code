import Prelude hiding(Right, Left)
import Data.Maybe (listToMaybe)
import Data.List.Split
import IntCodes
import Util
import qualified Data.Map.Strict as M

type Point = (Int, Int)
data Direction = Up | Right | Down | Left deriving (Show, Enum, Eq)
data Robot = Robot {
    program :: Program,
    hull :: M.Map Point Integer,
    facing :: Direction,
    position :: Point
} deriving Show

initRobot :: IntCodes -> Robot
initRobot codes = Robot {
    program = initProgram codes [],
    hull = M.empty,    
    facing = Up,
    position = (0, 0)
}

paintHull :: Robot -> Robot
paintHull r@(Robot { program = Program { terminated = True }}) = r
paintHull r = paintHull $ paintNext r

paintNext :: Robot -> Robot
paintNext r@(Robot { program = Program { terminated = True }}) = r
paintNext r = update $ (output prog)
    where        
        update (d:c:[]) = turn (r { 
            program = prog { output = [] },
            hull = M.insert (position r) c (hull r)}) (fromIntegral d)
        update _ = r { program = prog }
        prog = nextOutput $ nextOutput $ (program r) { input = [toInteger color] }        
        color = M.findWithDefault 0 (position r) (hull r)

turn :: Robot -> Int -> Robot
turn robot n = robot { facing = newFacing, position = translate1 newFacing $ position robot }
    where 
        newFacing = dir n $ facing robot
        dir 0 Up = Left
        dir 0 d = pred d
        dir 1 Left = Up
        dir 1 d = succ d

translate1 :: Direction -> Point -> Point
translate1 Up point = (fst point, snd point + 1)
translate1 Down point = (fst point, snd point - 1)
translate1 Right point = (fst point + 1, snd point)
translate1 Left point = (fst point - 1, snd point)

part1 :: Robot -> String
part1 = show . M.size . hull . paintHull

part2 :: Robot -> String
part2 r = unlines $ chunksOf (maxX - minX + 1) $ map (asChar . flip (M.findWithDefault 0) hullMap) [(x, y) | y <- [maxY, maxY-1..minY], x <- [minX..maxX]]
    where
        asChar 0 = ' '
        asChar 1 = '#'
        maxX = valueOf maximum fst + 1
        maxY = valueOf maximum snd + 1
        minX = valueOf minimum fst - 1
        minY = valueOf minimum snd - 1
        valueOf acc which = acc $ map (which . fst) $ M.toList hullMap
        hullMap = hull $ paintHull $ r { hull = M.insert (0, 0) 1 (hull r) }

parseInput :: String -> IntCodes
parseInput = read . ('[':) . (++"]")

main = interact $ part2 . initRobot . parseInput