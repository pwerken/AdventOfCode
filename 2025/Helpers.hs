module Helpers where

solve :: Show a => String -> (String -> a) -> IO ()
solve nr solver = let filename = "input-day" ++ nr ++ ".txt"
                   in readFile filename >>= putStrLn . show . solver
