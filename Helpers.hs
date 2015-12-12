module Helpers where

import Data.List

solve :: Show a => String -> (String -> a) -> IO ()
solve filename solver =
  do  input <- readFile filename
      putStrLn . show . solver $ input

splitOn x [] = []
splitOn x xs = first : splitOn x rest
  where
    first = takeWhile (/= x) xs
    rest  = drop (1 + length first) xs
