module Helpers where

import Data.List

solve :: String -> (String -> String) -> IO ()
solve filename solver = readFile filename >>= putStrLn . solver

split :: Eq a => a -> [a] -> ([a], [a])
split x = mapSnd (drop 1) . span (/= x)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x xs = first : splitOn x rest
  where
    (first, rest) = split x xs

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapFst f (x, y) = (f x, y)
mapSnd f (x, y) = (x, f y)
