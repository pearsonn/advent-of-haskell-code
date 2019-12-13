module Util where

fileInteract :: String -> (String -> String) -> IO ()
fileInteract file f = do
    input <- readFile file
    putStrLn (f input)
    
mapFst :: (a -> c) -> (a, b) -> (c, b)   
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)