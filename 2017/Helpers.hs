module Helpers where

import Data.List

solve :: Show a => String -> (String -> a) -> IO ()
solve nr solver = let filename = "input-day" ++ nr ++ ".txt"
                   in readFile filename >>= putStrLn . show . solver

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
