--- Day 11: Hex Ed ---

Crossing the bridge, you've barely reached the other side of the stream
when a program comes up to you, clearly in distress. "It's my child
process," she says, "he's gotten lost in an infinite grid!"

Fortunately for her, you have plenty of experience with infinite grids.

Unfortunately for you, it's a hex grid.

The hexagons ("hexes") in this grid are aligned such that adjacent hexes
can be found to the north, northeast, southeast, south, southwest, and
northwest:

          \ n  /
        nw +--+ ne
          /    \
        -+      +-
          \    /
        sw +--+ se
          / s  \

You have the path the child process took. Starting where he started, you
need to determine the fewest number of steps required to reach him. (A
"step" means to move from the hex you are in to any adjacent hex.)

For example:

  - ne,ne,ne is 3 steps away.
  - ne,ne,sw,sw is 0 steps away (back where you started).
  - ne,ne,s,s is 2 steps away (se,se).
  - se,sw,se,sw,sw is 3 steps away (s,s,sw).

>
> import Helpers
> import Data.Char
>
> data H = N | NE | SE | S | SW | NW
>   deriving (Read, Show)
> type Pos = (Int, Int, Int)
>
> parse :: String -> [H]
> parse = map read . splitOn ',' . map toUpper
>
> move :: Pos -> H -> Pos
> move (x, y, z) N  = (x + 1, y, z - 1)
> move (x, y, z) S  = (x - 1, y, z + 1)
> move (x, y, z) NE = (x + 1, y - 1, z)
> move (x, y, z) SW = (x - 1, y + 1, z)
> move (x, y, z) NW = (x, y + 1, z - 1)
> move (x, y, z) SE = (x, y - 1, z + 1)
>
> moveAll :: [H] -> Pos
> moveAll = foldl move (0, 0, 0)
>
> dist :: Pos -> Int
> dist (x, y, z) = (abs(x) + abs (y) + abs(z)) `div` 2
>
> day11 = solve "11" (dist . moveAll . parse)


--- Part Two ---

How many steps away is the furthest he ever got from his starting position?

>
> allPos :: [H] -> [Pos]
> allPos = scanl move (0,0,0)
>
> maxDist :: [Pos] -> Int
> maxDist = maximum . map dist
>
> day11p2 = solve "11" (maxDist . allPos . parse)
