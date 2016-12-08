--- Day 1: No Time for a Taxicab ---

Santa's sleigh uses a very high-precision clock to guide its movements, and
the clock's oscillator is regulated by stars. Unfortunately, the stars have
been stolen... by the Easter Bunny. To save Christmas, Santa needs you to
retrieve all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each
day in the advent calendar; the second puzzle is unlocked when you complete
the first. Each puzzle grants one star. Good luck!

You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near",
unfortunately, is as close as you can get - the instructions on the Easter
Bunny Recruiting Document the Elves intercepted start here, and nobody had
time to work them out further.

The Document indicates that you should start at the given coordinates (where
you just landed) and face North. Then, follow the provided sequence: either
turn left (L) or right (R) 90 degrees, then walk forward the given number of
blocks, ending at a new intersection.

There's no time to follow such ridiculous instructions on foot, though, so you
take a moment and work out the destination. Given that you can only walk on
the street grid of the city, how far is the shortest path to the destination?

For example:

-   Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5
    blocks away.
-   R2, R2, R2 leaves you 2 blocks due South of your starting position,
    which is 2 blocks away.
-   R5, L5, R5, R3 leaves you 12 blocks away.

How many blocks away is Easter Bunny HQ?

> import Helpers
>
> type Location = (Compass, Int, Int)
> data Compass = N | E | S | W
> data Turn = L | R
>
> dist (x, y) = show $ abs(x) + abs(y)
>
> makeTurn :: Compass -> Turn -> Compass
> makeTurn N L = W
> makeTurn E L = N
> makeTurn S L = E
> makeTurn W L = S
> makeTurn N R = E
> makeTurn E R = S
> makeTurn S R = W
> makeTurn W R = N
>
> turnsToMoves :: Compass -> [(Turn, Int)] -> [Compass]
> turnsToMoves _ [] = []
> turnsToMoves c ((t,0):xs) = turnsToMoves (makeTurn c t) xs
> turnsToMoves c ((t,d):xs) = (makeTurn c t) : turnsToMoves c ((t,d-1):xs)
>
> makeMove :: (Int, Int) -> Compass -> (Int, Int)
> makeMove (x, y) N = (x    , y + 1)
> makeMove (x, y) E = (x + 1, y    )
> makeMove (x, y) S = (x    , y - 1)
> makeMove (x, y) W = (x - 1, y    )
>
> makeMoves :: [Compass] -> (Int, Int)
> makeMoves = foldl makeMove (0,0)
>
> parse :: String -> [Compass]
> parse = turnsToMoves N . map parseD . words . filter (/= ',')
>
> parseD :: String -> (Turn, Int)
> parseD ('R':xs) = (R, read xs)
> parseD ('L':xs) = (L, read xs)
>
> day01 = solve "input-day01.txt" (dist . makeMoves . parse)


--- Part Two ---

Then, you notice the instructions continue on the back of the Recruiting
Document. Easter Bunny HQ is actually at the first location you visit twice.

For example, if your instructions are R8, R4, R4, R8, the first location you
visit twice is 4 blocks away, due East.

How many blocks away is the first location you visit twice?

> findDouble [x]     = x
> findDouble (x:xs)  = findDouble (limitSearch x xs)
>
> limitSearch _ []     = []
> limitSearch y (x:xs) = if x /= y then x : limitSearch y xs else [x]
>
> visits :: [Compass] -> [(Int, Int)]
> visits = scanl makeMove (0,0)
>
> day01p2 = solve "input-day01.txt" (dist . findDouble . visits . parse)
