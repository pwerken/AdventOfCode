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

maxWith :: Ord b => (a -> b) -> [a] -> a
maxWith f xs = snd . foldl1 g . map (\x -> (f x, x)) $ xs
  where
    g :: Ord a => (a, b) -> (a, b) -> (a, b)
    g x y = if (fst x) > (fst y) then x else y

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = sortBy (\a b -> compare (f a) (f b))

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapFst f (x, y) = (f x, y)
mapSnd f (x, y) = (x, f y)
