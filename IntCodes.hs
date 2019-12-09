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

ops = [(1, 3), (2, 3), (3, 1), (4, 1)]

execute :: Program -> Instruction -> Program
execute program (Instruction 99 _ ) = program { terminated = True }
execute program (Instruction op params) =
    let f 1 ((p1, m1):(p2, m2):[]) = undefined
        f 2 ((p1, m1):(p2, m2):[]) = undefined
        f 3 ((p, _):i:[]) = program
        f 4 ((p, m):i:[]) = program { output = value p m : output program }
        f _ _ = error "Unknown opcode"
    in f op params
    where 
        value param Pos = codes program I.! param
        value param _ = param

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

runProgram :: IntCodes -> [Int] -> [Int]
runProgram codes input = output $ runProgram' (Program (I.fromList (zip [0..] codes)) 0 input [] False)
        where 
            runProgram' p@(Program { terminated = True }) = p --base case to unwrap recursion
            runProgram' p = runProgram' $ execute p $ nextInstruction p --recursively execute next instruction