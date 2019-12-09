module IntCodes where

import Data.Char
import Data.Maybe (fromMaybe)
import qualified Data.IntMap.Strict as I

type IntCodes = [Int]
type Param = (Int, Mode)
type OpCode = Int

data Mode = Pos | Im deriving Show
data Instruction = Instruction Int [Param] deriving Show
data Program = Program { codes :: I.IntMap Int
                       , ptr :: Int
                       , input :: [Int]
                       , output :: [Int]
                       , terminated :: Bool
                       } deriving Show

ops = [(1, 3), (2, 3), (3, 1), (4, 1), (5, 2), (6, 2), (7, 3), (8, 3)]

initProgram :: IntCodes -> [Int] -> Program
initProgram codes input = Program (I.fromList (zip [0..] codes)) 0 input [] False

execute :: Program -> Instruction -> Program
execute program (Instruction 99 _ ) = program { terminated = True }
execute program (Instruction op params) =
    let f 1 p = executeBinary (+) p
        f 2 p = executeBinary (*) p
        f 3 (p:[]) = program { 
            codes = store (head (input program)) p, 
            input = tail (input program), 
            ptr = ptr program + 2 }
        f 4 ((p, m):[]) = program { 
            output = valueOf p m : output program, 
            ptr = ptr program + 2 }
        f 5 (p1:p2:[]) = jumpIf (/=0) p1 p2 
        f 6 (p1:p2:[]) = jumpIf (==0) p1 p2
        f 7 p = executeBinary ((boolToInt .) . (<)) p
        f 8 p = executeBinary ((boolToInt .) . (==)) p
        f _ _ = error "Unknown opcode"
        boolToInt True = 1
        boolToInt False = 0
    in f op params 
    where
        executeBinary f ((p1, m1):(p2, m2):c:[]) = program { codes = store newValue c, ptr = ptr program + 4}
            where newValue = f (valueOf p1 m1) (valueOf p2 m2)
        jumpIf predicate (p1, m1) (p2, m2)
            | predicate (valueOf p1 m1) = program { ptr = valueOf p2 m2 }
            | otherwise = program { ptr = ptr program + 3 }
        store val (c1,Pos) = I.adjust (const val) c1 (codes program)
        store _ (_,Im) = error "Store immediate doesn't make sense"
        valueOf param Pos = codes program I.! param
        valueOf param _ = param

nextInstruction :: Program -> Instruction
nextInstruction (Program codes ptr _ _ _) = Instruction opCode params
    where        
        params = zip (map (codes I.!) $ tail [ptr..ptr + paramCount]) (modes values)
        paramCount = fromMaybe 0 (lookup opCode ops)
        modes = map (mode. digitToInt) . drop 2 . reverse
        opCode = read $ drop 3 values
        values = replicate (5 - length op) '0' ++ op
        op = show $ codes I.! ptr
        mode 0 = Pos
        mode 1 = Im

testInput :: [Int]
testInput = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]

runProgram :: IntCodes -> [Int] -> [Int]
runProgram codes input = output $ runProgram' $ initProgram codes input
        where 
            runProgram' p@(Program { terminated = True }) = p --base case to unwrap recursion
            runProgram' p = runProgram' $ execute p $ nextInstruction p --recursively execute next instruction