import Data.Char
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

type IntCodes = [Int]
type Param = (Int, Mode)
type OpCode = Int

data Mode = Pos | Im deriving Show
data Instruction = Instruction OpCode [Param] deriving Show
data Program = Program { codes :: V.Vector Int
                       , ptr :: Int
                       , input :: Int
                       , output :: [Int]
                       , terminated :: Bool
                       } deriving Show

ops = [(1, 3), (2, 3), (3, 1), (4, 1)] --association list of opCode -> number of params

part1 :: IntCodes -> Int -> [Int]
part1 = undefined

execute :: Program -> Instruction -> Program
execute program (Instruction 99 _ ) = program { terminated = True }
execute program (Instruction op params) =
    let f 1 ((p1, m1):(p2, m2):[]) = undefined
        f 2 ((p1, m1):(p2, m2):[]) = undefined
        f 3 ((p, _):i:[]) = program { codes =  }
        f 4 ((p, m):i:[]) = program { output = value p m : output program }
        f _ _ = error "Unknown opcode"
    in f op params
    where 
        value param Pos = codes program V.! param
        value param _ = param

nextInstruction :: Program -> Instruction
nextInstruction (Program codes ptr _ _ _) = Instruction opCode params
    where
        params = map (\p -> (snd p, (mode (fst p)))) $ zip ((map digitToInt . drop 2 . reverse) values) (V.toList $ V.slice (ptr + 1) paramCount codes)
        paramCount = fromMaybe 0 (lookup opCode ops)
        opCode = read $ drop 3 values
        values = replicate (5 - length (show op)) '0' ++ show op
        op = codes V.! ptr
        mode 0 = Pos
        mode 1 = Im