--- Day 16: Permutation Promenade ---

You come upon a very unusual sight; a group of programs here appear to be
dancing.

There are sixteen programs in total, named a through p. They start by
standing in a line: a stands in position 0, b stands in position 1, and so
on until p, which stands in position 15.

The programs' dance consists of a sequence of dance moves:

  - Spin, written sX, makes X programs move from the end to the front, but
    maintain their order otherwise. (For example, s3 on abcde produces cdeab
    ).
  - Exchange, written xA/B, makes the programs at positions A and B swap
    places.
  - Partner, written pA/B, makes the programs named A and B swap places.

For example, with only five programs standing in a line (abcde), they could
do the following dance:

  - s1, a spin of size 1: eabcd.
  - x3/4, swapping the last two programs: eabdc.
  - pe/b, swapping programs e and b: baedc.

After finishing their dance, the programs end up in order baedc.

You watch the dance for a while and record their dance moves (your puzzle
input). In what order are the programs standing after their dance?

>
> import Helpers
> import Data.Char (ord)
> import Data.List (sort, foldl')
>
> data Action = S Int | X Int Int | P Char Char
>   deriving Show
>
> ap = ['a'..'p']
>
> parse :: String -> [Action]
> parse = map parse' . splitOn ','
>   where
>     parse' ('s':xs) = S (read xs)
>     parse' ('x':xs) = (\[x,y] -> X x y) . sort . map read . splitOn '/' $ xs
>     parse' ('p':xs) = (\[x,y] -> P x y) . map head . splitOn '/' $ xs
>
> step :: Action -> [Char] -> [Char]
> step (S x)   = take 16 . drop (16 - x) . cycle
> step (X a b) = let f (as, ((b:bs), (c:cs))) = as ++ (c:bs) ++ (b:cs)
>                 in f . mapSnd (splitAt (b - a)) . splitAt a
> step (P a b) = let f (as, (bs, (cs, (ds, es)))) = concat [as,ds,cs,bs,es]
>                    g = mapSnd (splitAt 1) . break (`elem` [a, b])
>                 in f . mapSnd (mapSnd g) . g
>
> dance is = foldl' (flip step) is
>
> day16 = solve "16" (dance ap . parse)

-- Part Two ---

Now that you're starting to get a feel for the dance moves, you turn your
attention to the dance as a whole.

Keeping the positions they ended up in from their previous dance, the
programs perform it again and again: including the first dance, a total of
one billion (1000000000) times.

In the example above, their second dance would begin with the order baedc,
and use the same dance moves:

    s1, a spin of size 1: cbaed.
    x3/4, swapping the last two programs: cbade.
    pe/b, swapping programs e and b: ceadb.

In what order are the programs standing after their billion dances?

>
> billionth :: ([a], [a]) -> a
> billionth (as, bs) = bs !! ((1000000000 - length as) `mod` (length bs))
>
> findPattern :: Eq a => [a] -> [a] -> ([a], [a])
> findPattern ms (a:as)
>   | a `elem` ms   = mapSnd (a:) . split a . reverse $ ms
>   | otherwise     = findPattern (a:ms) as
>
> forever :: [Action] -> [[Char]]
> forever as = iterate (flip dance as) ap
>
> day16p2 = solve "16" (billionth . findPattern [] . forever . parse)
