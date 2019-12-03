module Util where

fileInteract :: String -> (String -> String) -> IO ()
fileInteract file f = do
    input <- readFile file
    putStrLn (f input)