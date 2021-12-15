--- Day 5: Hydrothermal Venture ---

You come across a field of hydrothermal vents on the ocean floor! These
vents constantly produce large, opaque clouds, so it would be best to avoid
them if possible.

They tend to form in lines; the submarine helpfully produces a list of
nearby lines of vents (your puzzle input) for you to review. For example:

    0,9 -> 5,9
    8,0 -> 0,8
    9,4 -> 3,4
    2,2 -> 2,1
    7,0 -> 7,4
    6,4 -> 2,0
    0,9 -> 2,9
    3,4 -> 1,4
    0,0 -> 8,8
    5,5 -> 8,2

Each line of vents is given as a line segment in the format x1,y1 -> x2,y2
where x1,y1 are the coordinates of one end the line segment and x2,y2 are
the coordinates of the other end. These line segments include the points at
both ends. In other words:

  - An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
  - An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.

For now, only consider horizontal and vertical lines: lines where either
x1 = x2 or y1 = y2.

So, the horizontal and vertical lines from the above list would produce the
following diagram:

    .......1..
    ..1....1..
    ..1....1..
    .......1..
    .112111211
    ..........
    ..........
    ..........
    ..........
    222111....

In this diagram, the top left corner is 0,0 and the bottom right corner is
9,9. Each position is shown as the number of lines which cover that point
or . if no line covers that point. The top-left pair of 1s, for example,
comes from 2,2 -> 2,1; the very bottom row is formed by the overlapping
lines 0,9 -> 5,9 and 0,9 -> 2,9.

To avoid the most dangerous areas, you need to determine the number of
points where at least two lines overlap. In the above example, this is
anywhere in the diagram with a 2 or larger - a total of 5 points.

Consider only horizontal and vertical lines. At how many points do at least
two lines overlap?

>
> import Helpers
> import Data.List
>
> type Pos = (Int, Int)
> type Line = (Pos, Pos)
>
> parse :: String -> [Line]
> parse = map (toLine . map read . words) . lines . cleanUp
>   where
>     cleanUp :: String -> String
>     cleanUp = map comma2space . filter ((/=) '>') . filter ((/=) '-')
>
>     comma2space :: Char -> Char
>     comma2space ',' = ' '
>     comma2space c   = c
>
>     toLine :: [Int] -> Line
>     toLine [a,b,c,d] = ((a, b), (c, d))
>
> isHoriz :: Line -> Bool
> isHoriz ((a, _), (c, _)) = (a == c)
>
> isVert :: Line -> Bool
> isVert ((_, b), (_, d)) = (b == d)
>
> onlyVH :: [Line] -> [Line]
> onlyVH = filter (\l -> isHoriz l || isVert l)
>
> drawLine1 :: Line -> [Pos]
> drawLine1 ((a, b), (c, d))
>   | (a == c) && (b == d)  = [(a, b)]
>   | (a == c) && (b < d)   = (a, b) : drawLine1 ((a, b+1), (c, d))
>   | (a == c) && (b > d)   = (a, b) : drawLine1 ((a, b-1), (c, d))
>   | (a < c)               = (a, b) : drawLine1 ((a+1, b), (c, d))
>   | (a > c)               = (a, b) : drawLine1 ((a-1, b), (c, d))
>
> drawAllLines1 :: [Line] -> [Pos]
> drawAllLines1 = sort . concatMap drawLine1 . onlyVH
>
> solveP1 = length . filter (> 1) . map length . group . drawAllLines1
>
> day05 = solve "05" (solveP1 . parse)


--- Part Two ---

Unfortunately, considering only horizontal and vertical lines doesn't give
you the full picture; you need to also consider diagonal lines.

Because of the limits of the hydrothermal vent mapping system, the lines in
your list will only ever be horizontal, vertical, or a diagonal line at
exactly 45 degrees. In other words:

  - An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
  - An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.

Considering all lines from the above example would now produce the
following diagram:

    1.1....11.
    .111...2..
    ..2.1.111.
    ...1.2.2..
    .112313211
    ...1.2....
    ..1...1...
    .1.....1..
    1.......1.
    222111....

You still need to determine the number of points where at least two lines
overlap. In the above example, this is still anywhere in the diagram with a
2 or larger - now a total of 12 points.

Consider all of the lines. At how many points do at least two lines
overlap?

>
> isDiag :: Line -> Bool
> isDiag ((a, b), (c, d)) = abs(a - c) == abs(b - d)
>
> drawLine2 :: Line -> [Pos]
> drawLine2 ((a, b), (c, d))
>   | (a == c) && (b == d)  = [(a, b)]
>   | (a == c) && (b < d)   = (a, b) : drawLine2 ((a  , b+1), (c, d))
>   | (a == c) && (b > d)   = (a, b) : drawLine2 ((a  , b-1), (c, d))
>   | (a < c)  && (b == d)  = (a, b) : drawLine2 ((a+1, b  ), (c, d))
>   | (a > c)  && (b == d)  = (a, b) : drawLine2 ((a-1, b  ), (c, d))
>   | (a < c)  && (b < d)   = (a, b) : drawLine2 ((a+1, b+1), (c, d))
>   | (a < c)  && (b > d)   = (a, b) : drawLine2 ((a+1, b-1), (c, d))
>   | (a > c)  && (b < d)   = (a, b) : drawLine2 ((a-1, b+1), (c, d))
>   | (a > c)  && (b > d)   = (a, b) : drawLine2 ((a-1, b-1), (c, d))
>
> onlyStraigth :: [Line] -> [Line]
> onlyStraigth = filter (\l -> isHoriz l || isVert l || isDiag l)
>
> drawAllLines2 :: [Line] -> [Pos]
> drawAllLines2 = sort . concatMap drawLine2 . onlyStraigth
>
> solveP2 = length . filter (> 1) . map length . group . drawAllLines2
>
> day05p2 = solve "05" (solveP2 . parse)
>
> exampleData :: [Line]
> exampleData =
>   [ ((0,9), (5,9))
>   , ((8,0), (0,8))
>   , ((9,4), (3,4))
>   , ((2,2), (2,1))
>   , ((7,0), (7,4))
>   , ((6,4), (2,0))
>   , ((0,9), (2,9))
>   , ((3,4), (1,4))
>   , ((0,0), (8,8))
>   , ((5,5), (8,2))
>   ]
