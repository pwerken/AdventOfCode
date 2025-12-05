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
mapFst = (flip mapTuple) id

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd = mapTuple id

mapTuple :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapTuple f g (x, y) = (f x, g y)

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)

chuncksOf :: Int -> [a] -> [[a]]
chuncksOf _ [] = []
chuncksOf i xs = uncurry (:) . mapSnd (chuncksOf i) $ splitAt i xs

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent _ (_:[]) = []
mapAdjacent f (a:b:cs) = f a b : mapAdjacent f (b:cs)
