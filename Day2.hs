input = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,6,19,23,2,23,6,27,1,5,27,31,1,31,9,35,2,10,35,39,1,5,39,43,2,43,10,47,1,47,6,51,2,51,6,55,2,55,13,59,2,6,59,63,1,63,5,67,1,6,67,71,2,71,9,75,1,6,75,79,2,13,79,83,1,9,83,87,1,87,13,91,2,91,10,95,1,6,95,99,1,99,13,103,1,13,103,107,2,107,10,111,1,9,111,115,1,115,10,119,1,5,119,123,1,6,123,127,1,10,127,131,1,2,131,135,1,135,10,0,99,2,14,0,0]

run :: Int -> [Int] -> [Int] -> [Int]
run p program (1:a:b:c:_) = run (p + 4) (execute program (+) a b c) (drop (p + 4) program)
run p program (2:a:b:c:_) = run (p + 4) (execute program (*) a b c) (drop (p + 4) program)
run _ program (99:_) = program

run2 program = 100 * n + v
    where 
        (n, v) = head [(i, j) | i <- [0..99], j <- [0..99], let p = set i j program, head (run 0 p p) == 19690720]
        set noun verb (x:_:_:xs) = x:noun:verb:xs    

execute :: [a] -> (a -> a -> a) -> Int -> Int -> Int -> [a]
execute program f a b c = init ++ c':tail
    where        
        init = take c program
        tail = drop (c + 1) program
        c' = f a' b'
        a' = program !! a
        b' = program !! b

main = putStrLn $ show $ run2 input