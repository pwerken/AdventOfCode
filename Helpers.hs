module Helpers where

import Data.List

solve :: Show a => String -> (String -> a) -> IO ()
solve filename solver =
  readFile filename >>= putStrLn . show . solver

splitOn x [] = []
splitOn x xs = first : splitOn x (drop 1 rest)
  where
    (first, rest) = span (/= x) xs

mapFst :: (a, b) -> (a -> c) -> (c, b)
mapSnd :: (a, b) -> (b -> c) -> (a, c)
mapFst (x, y) f = (f x, y)
mapSnd (x, y) f = (x, f y)
