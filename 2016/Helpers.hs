module Helpers where

import Data.List

solve :: String -> (String -> String) -> IO ()
solve filename solver = readFile filename >>= putStrLn . solver

splitOn x [] = []
splitOn x xs = first : splitOn x (drop 1 rest)
  where
    (first, rest) = span (/= x) xs

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapFst f (x, y) = (f x, y)
mapSnd f (x, y) = (x, f y)
