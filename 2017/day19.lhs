--- Day 19: A Series of Tubes ---

Somehow, a network packet got lost and ended up here. It's trying to follow
a routing diagram (your puzzle input), but it's confused about where to go.

Its starting point is just off the top of the diagram. Lines (drawn with |,
-, and +) show the path it needs to take, starting by going down onto the
only line connected to the top of the diagram. It needs to follow this path
until it reaches the end (located somewhere within the diagram) and stop
there.

Sometimes, the lines cross over each other; in these cases, it needs to
continue going the same direction, and only turn left or right when there's
no other option. In addition, someone has left letters on the line; these
also don't change its direction, but it can use them to keep track of where
it's been. For example:

             |
             |  +--+
             A  |  C
         F---|----E|--+
             |  |  |  D
             +B-+  +--+

Given this diagram, the packet needs to take the following path:

  - Starting at the only line touching the top of the diagram, it must go
    down, pass through A, and continue onward to the first +.
  - Travel right, up, and right, passing through B in the process.
  - Continue down (collecting C), right, and up (collecting D).
  - Finally, go all the way left through E and stopping at F.

Following the path to the end, the letters it sees on its path are ABCDEF.

The little packet looks up at you, hoping you can help it find the way. What
letters will it see (in the order it would see them) if it follows the path?
(The routing diagram is very wide; make sure you view it without line
wrapping.)

>
> import Helpers
> import Data.List
>
> type Pos = (Int, Int)
> type Segments = [((Pos, Pos), String)]
>
> -- parse :: [[Char]] -> [(Segment, String)]
> parse :: String -> [[(Pos, Char)]]
> parse = filter (not . null) . zipWith (parse' 0) [0..] . lines
>   where
>     parse'  _ _ []     = []
>     parse'  x y (c:cs) = ((x, y), c) : parse' (x + 1) y cs
>
> isLetter :: Char -> Bool
> isLetter = flip elem ['A'..'Z']
>
> toSegments :: [[(Pos, Char)]] -> Segments
> toSegments ps = let hs = concat . map toHSegs' $ ps
>                     vs = concat . map toVSegs  . transpose $ ps
>                  in hs ++ vs
>   where
>     toVSegs ((x, c):rs)
>       | c == '|'    = toVSegs'' ((-1, -1), []) rs
>       | otherwise   = toVSegs'  rs
>     toVSegs' []     = []
>     toVSegs' ((x, c):rs)
>       | c == '+'    = toVSegs'' (x, []) rs
>       | isLetter c  = toVSegs'' (x, [c]) rs
>       | otherwise   = toVSegs'  rs
>     toVSegs'' _ []  = []
>     toVSegs'' (x, s) ((y, c):rs)
>       | c == '+'    = ((x, y), reverse s) : toVSegs' rs
>       | isLetter c  = toVSegs'' (x, c:s) rs
>       | otherwise   = toVSegs'' (x, s) rs
>     toHSegs' []     = []
>     toHSegs' ((x, c):rs)
>       | c == '+'    = toHSegs'' (x, []) rs
>       | otherwise   = toHSegs' rs
>     toHSegs'' (x, s) ((y, c):rs)
>       | c == '+'    = ((x, y), reverse s) : toHSegs' rs
>       | isLetter c  = toHSegs'' (x, c:s) rs
>       | otherwise   = toHSegs'' (x, s) rs
>
> findPos :: (Segments, (Pos, String)) -> (Segments, (Pos, String))
> findPos ([], z)   = ([], z)
> findPos ((r@((x, y), s):rs), z)
>   | x == (fst z)  = (rs, (y, s))
>   | y == (fst z)  = (rs, (x, s))
>   | otherwise     = mapFst (r:) $ findPos (rs, z)
>
> follow :: Segments -> [(Pos, String)]
> follow rs = follow' . map snd . iterate findPos $ (rs, s)
>   where
>     s = ((-1, -1), [])
>     follow' ((p, s):rs@((r,_):_))
>       | p == r    = [(p, s)]
>       | otherwise = (p, s) : follow' rs
>
> day19 = solve "19" (concatMap snd . follow . toSegments . parse)


--- Part Two ---

The packet is curious how many steps it needs to go.

For example, using the same routing diagram from the example above...

             |
             |  +--+
             A  |  C
         F---|--|-E---+
             |  |  |  D
             +B-+  +--+

...the packet would go:

  - 6 steps down (including the first line at the top of the diagram).
  - 3 steps right.
  - 4 steps up.
  - 3 steps right.
  - 4 steps down.
  - 3 steps right.
  - 2 steps up.
  - 13 steps left (including the F it stops on).

This would result in a total of 38 steps.

How many steps does the packet need to go?

>
> steps (a:[])   = 0
> steps (a:b:cs) = distPos a b + steps (b:cs)
>
> distPos :: Pos -> Pos -> Int
> distPos (-1, -1) (x, y)   = y + 1
> distPos (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
>
> day19p2 = solve "19" (steps . map fst . follow . toSegments . parse)
