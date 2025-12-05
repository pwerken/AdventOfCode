--- Day 4: Printing Department ---

You ride the escalator down to the printing department. They're clearly
getting ready for Christmas; they have lots of large rolls of paper
everywhere, and there's even a massive printer in the corner (to handle the
really big print jobs).

Decorating here will be easy: they can make their own decorations. What you
really need is a way to get further into the North Pole base while the
elevators are offline.

"Actually, maybe we can help with that," one of the Elves replies when you ask
for help. "We're pretty sure there's a cafeteria on the other side of the back
wall. If we could break through the wall, you'd be able to keep moving. It's
too bad all of our forklifts are so busy moving those big rolls of paper
around."

If you can optimize the work the forklifts are doing, maybe they would have
time to spare to break through the wall.

The rolls of paper (@) are arranged on a large grid; the Elves even have a
helpful diagram (your puzzle input) indicating where everything is located.

For example:

    ..@@.@@@@.
    @@@.@.@.@@
    @@@@@.@.@@
    @.@@@@..@.
    @@.@@@@.@@
    .@@@@@@@.@
    .@.@.@.@@@
    @.@@@.@@@@
    .@@@@@@@@.
    @.@.@@@.@.

The forklifts can only access a roll of paper if there are fewer than four
rolls of paper in the eight adjacent positions. If you can figure out which
rolls of paper the forklifts can access, they'll spend less time looking and
more time breaking down the wall to the cafeteria.

In this example, there are 13 rolls of paper that can be accessed by a
forklift (marked with x):

    ..xx.xx@x.
    x@@.@.@.@@
    @@@@@.x.@@
    @.@@@@..@.
    x@.@@@@.@x
    .@@@@@@@.@
    .@.@.@.@@@
    x.@@@.@@@@
    .@@@@@@@@.
    x.x.@@@.x.

Consider your complete diagram of the paper roll locations. How many rolls of
paper can be accessed by a forklift?

>
> import Helpers
> import qualified Data.Map.Strict as Map
>
> type Pos = (Int, Int)
> type Grid = Map.Map Pos Bool
>
> parse :: String -> Grid
> parse = Map.fromList . toGrid . toBool . lines
>   where
>     toBool :: [[Char]] -> [[Bool]]
>     toBool = map (map ((==) '@'))
>
>     toGrid :: [[a]] -> [(Pos, a)]
>     toGrid = concat . gridY . gridX
>
>     addPos :: a -> Int -> Int -> (Pos, a)
>     addPos v x y = ((x, y), v)
>
>     gridX :: [[a]] -> [[Int -> (Pos, a)]]
>     gridX = map (zipWith (flip addPos) [1..])
>
>     gridY :: [[Int -> (Pos, a)]] -> [[(Pos, a)]]
>     gridY = zipWith (\x -> map ($ x)) [1..]
>
> posToSquare :: Pos -> [Pos]
> posToSquare (x, y) = [(a, b) | a <- [x-1, x, x+1], b <- [y-1,y,y+1]]
>
> posToNeighbours :: Pos -> [Pos]
> posToNeighbours p = filter (/= p) . posToSquare $ p
>
> isFilled :: Grid -> Pos -> Bool
> isFilled g p = or $ Map.lookup p g
>
> neighbours :: Grid -> Pos -> [Pos]
> neighbours g p = filter (isFilled g) . posToNeighbours $ p
>
> isAccessible :: Grid -> Pos -> Bool
> isAccessible g = (< 4) . length . neighbours g
>
> allAccessible :: Grid -> [Pos]
> allAccessible g = filter (isAccessible g) . filter (isFilled g) . Map.keys $ g
>
> day04p1 = solve "04" (length . allAccessible . parse)

--- Part Two ---
Now, the Elves just need help accessing as much of the paper as they can.

Once a roll of paper can be accessed by a forklift, it can be removed. Once a
roll of paper is removed, the forklifts might be able to access more rolls of
paper, which they might also be able to remove. How many total rolls of paper
could the Elves remove if they keep repeating this process?

Starting with the same example as above, here is one way you could remove as
many rolls of paper as possible, using highlighted @ to indicate that a roll
of paper is about to be removed, and using x to indicate that a roll of paper
was just removed:

  Initial state:
  ..@@.@@@@.
  @@@.@.@.@@
  @@@@@.@.@@
  @.@@@@..@.
  @@.@@@@.@@
  .@@@@@@@.@
  .@.@.@.@@@
  @.@@@.@@@@
  .@@@@@@@@.
  @.@.@@@.@.

  Remove 13 rolls of paper:
  ..xx.xx@x.
  x@@.@.@.@@
  @@@@@.x.@@
  @.@@@@..@.
  x@.@@@@.@x
  .@@@@@@@.@
  .@.@.@.@@@
  x.@@@.@@@@
  .@@@@@@@@.
  x.x.@@@.x.

  Remove 12 rolls of paper:
  .......x..
  .@@.x.x.@x
  x@@@@...@@
  x.@@@@..x.
  .@.@@@@.x.
  .x@@@@@@.x
  .x.@.@.@@@
  ..@@@.@@@@
  .x@@@@@@@.
  ....@@@...

  Remove 7 rolls of paper:
  ..........
  .x@.....x.
  .@@@@...xx
  ..@@@@....
  .x.@@@@...
  ..@@@@@@..
  ...@.@.@@x
  ..@@@.@@@@
  ..x@@@@@@.
  ....@@@...

  Remove 5 rolls of paper:
  ..........
  ..x.......
  .x@@@.....
  ..@@@@....
  ...@@@@...
  ..x@@@@@..
  ...@.@.@@.
  ..x@@.@@@x
  ...@@@@@@.
  ....@@@...

  Remove 2 rolls of paper:
  ..........
  ..........
  ..x@@.....
  ..@@@@....
  ...@@@@...
  ...@@@@@..
  ...@.@.@@.
  ...@@.@@@.
  ...@@@@@x.
  ....@@@...

  Remove 1 roll of paper:
  ..........
  ..........
  ...@@.....
  ..x@@@....
  ...@@@@...
  ...@@@@@..
  ...@.@.@@.
  ...@@.@@@.
  ...@@@@@..
  ....@@@...

  Remove 1 roll of paper:
  ..........
  ..........
  ...x@.....
  ...@@@....
  ...@@@@...
  ...@@@@@..
  ...@.@.@@.
  ...@@.@@@.
  ...@@@@@..
  ....@@@...

  Remove 1 roll of paper:
  ..........
  ..........
  ....x.....
  ...@@@....
  ...@@@@...
  ...@@@@@..
  ...@.@.@@.
  ...@@.@@@.
  ...@@@@@..
  ....@@@...

  Remove 1 roll of paper:
  ..........
  ..........
  ..........
  ...x@@....
  ...@@@@...
  ...@@@@@..
  ...@.@.@@.
  ...@@.@@@.
  ...@@@@@..
  ....@@@...

Stop once no more rolls of paper are accessible by a forklift. In this
example, a total of 43 rolls of paper can be removed.

Start with your original diagram. How many rolls of paper in total can be
removed by the Elves and their forklifts?

>
> removePosFromGrid :: Grid -> Pos -> Grid
> removePosFromGrid g p = Map.insert p False g
>
> removeAllFromGrid :: Grid -> [Pos] -> Grid
> removeAllFromGrid = foldr (flip removePosFromGrid)
>
> removeAccessible :: Grid -> (Int, Grid)
> removeAccessible g = (length rs, removeAllFromGrid g rs)
>   where
>     rs = allAccessible g
>
> keepRemovingAccessible :: Grid -> [Int]
> keepRemovingAccessible g = let (i, h) = removeAccessible g
>                             in if i == 0
>                                then []
>                                else i : keepRemovingAccessible h
>
> day04p2 = solve "04" (sum . keepRemovingAccessible . parse)
