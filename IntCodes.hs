module IntCodes where

import Data.Char
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

type IntCodes = [Integer]
type Param = (Integer, Mode)
type OpCode = Int

data Mode = Position | Immediate | Relative deriving Show
data Instruction = Instruction Int [Param] deriving Show
data Program = Program { codes :: M.Map Integer Integer
                       , ptr :: Integer
                       , relativeBase :: Integer
                       , input :: [Integer]
                       , output :: [Integer]
                       , terminated :: Bool
                       } deriving Show

ops = [(1, 3), (2, 3), (3, 1), (4, 1), (5, 2), (6, 2), (7, 3), (8, 3), (9, 1)]

initProgram :: IntCodes -> [Integer] -> Program
initProgram codes input = Program (M.fromList (zip [0..] codes)) 0 0 input [] False

execute :: Program -> Instruction -> Program
execute program (Instruction 99 _ ) = program { terminated = True }
execute program (Instruction op params) =
    let f 1 p = executeBinary (+) p
        f 2 p = executeBinary (*) p
        f 3 (p:[])
            | null $ input program = error "Waiting for input"
            | otherwise = advancePtr 2 program { 
                codes = store (head (input program)) p, 
                input = tail (input program) }
        f 4 ((p, m):[]) = advancePtr 2 program { output = valueOf p m : output program }
        f 5 (p1:p2:[]) = jumpIf (/=0) p1 p2 
        f 6 (p1:p2:[]) = jumpIf (==0) p1 p2
        f 7 p = executeBinary ((boolToInt .) . (<)) p
        f 8 p = executeBinary ((boolToInt .) . (==)) p
        f 9 ((p, m):[]) = advancePtr 2 program { relativeBase = relativeBase program + valueOf p m }
        f _ _ = error "Unknown opcode"
    in f op params 
    where
        advancePtr num p = p { ptr = ptr p + num }
        executeBinary f ((p1, m1):(p2, m2):c:[]) = advancePtr 4 program { codes = store newValue c }
            where newValue = f (valueOf p1 m1) (valueOf p2 m2)
        jumpIf predicate (p1, m1) (p2, m2)
            | predicate (valueOf p1 m1) = program { ptr = valueOf p2 m2 }
            | otherwise = advancePtr 3 program
        store val (c1,Position) = M.insert c1 val (codes program)
        store val (c1,Relative) = M.insert (relativeBase program + c1) val (codes program)
        store _ (_,Immediate) = error "Store immediate doesn't make sense"
        valueOf param Position = M.findWithDefault 0 param (codes program)
        valueOf param Relative = M.findWithDefault 0 (relativeBase program + param) (codes program)
        valueOf param _ = param
        boolToInt True = 1
        boolToInt False = 0

nextInstruction :: Program -> Instruction
nextInstruction (Program codes ptr _ _ _ _) = Instruction opCode params
    where        
        params = zip (map (codes M.!) $ tail [ptr..ptr + paramCount]) (modes values)
        paramCount = fromMaybe 0 (lookup opCode ops)
        modes = map (mode. digitToInt) . drop 2 . reverse
        opCode = read $ drop 3 values
        values = replicate (5 - length op) '0' ++ op
        op = show $ codes M.! ptr
        mode 0 = Position
        mode 1 = Immediate
        mode 2 = Relative

runProgram :: IntCodes -> [Integer] -> [Integer]
runProgram codes input = output $ runProgram' $ initProgram codes input
        where 
            runProgram' p@(Program { terminated = True }) = p --base case to unwrap recursion
            runProgram' p = runProgram' $ execute p $ nextInstruction p --recursively execute next instruction