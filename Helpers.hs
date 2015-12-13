module Helpers where

import Data.List

solve :: Show a => String -> (String -> a) -> IO ()
solve filename solver =
  readFile filename >>= putStrLn . show . solver

splitOn x [] = []
splitOn x xs = first : splitOn x (drop 1 rest)
  where
    (first, rest) = span (/= x) xs

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapFst f (x, y) = (f x, y)
mapSnd f (x, y) = (x, f y)
