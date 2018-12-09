--- Day 9: Marble Mania ---

You talk to the Elves while you wait for your navigation system to
initialize. To pass the time, they introduce you to their favorite marble
game.

The Elves play this game by taking turns arranging the marbles in a circle
according to very particular rules. The marbles are numbered starting with 0
and increasing by 1 until every marble has a number.

First, the marble numbered 0 is placed in the circle. At this point, while
it contains only a single marble, it is still a circle: the marble is both
clockwise from itself and counter-clockwise from itself. This marble is
designated the current marble.

Then, each Elf takes a turn placing the lowest-numbered remaining marble
into the circle between the marbles that are 1 and 2 marbles clockwise of
the current marble. (When the circle is large enough, this means that there
is one marble between the marble that was just placed and the current
marble.) The marble that was just placed then becomes the current marble.

However, if the marble that is about to be placed has a number which is a
multiple of 23, something entirely different happens. First, the current
player keeps the marble they would have placed, adding it to their score. In
addition, the marble 7 marbles counter-clockwise from the current marble is
removed from the circle and also added to the current player's score. The
marble located immediately clockwise of the marble that was removed becomes
the new current marble.

For example, suppose there are 9 players. After the marble with value 0 is
placed in the middle, each player (shown in square brackets) takes a turn.
The result of each of those turns would produce circles of marbles like
this, where clockwise is to the right and the resulting current marble is in
parentheses:

    [-] (0)
    [1]  0 (1)
    [2]  0 (2) 1
    [3]  0  2  1 (3)
    [4]  0 (4) 2  1  3
    [5]  0  4  2 (5) 1  3
    [6]  0  4  2  5  1 (6) 3
    [7]  0  4  2  5  1  6  3 (7)
    [8]  0 (8) 4  2  5  1  6  3  7
    [9]  0  8  4 (9) 2  5  1  6  3  7
    [1]  0  8  4  9  2(10) 5  1  6  3  7
    [2]  0  8  4  9  2 10  5(11) 1  6  3  7
    [3]  0  8  4  9  2 10  5 11  1(12) 6  3  7
    [4]  0  8  4  9  2 10  5 11  1 12  6(13) 3  7
    [5]  0  8  4  9  2 10  5 11  1 12  6 13  3(14) 7
    [6]  0  8  4  9  2 10  5 11  1 12  6 13  3 14  7(15)
    [7]  0(16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15
    [8]  0 16  8(17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15
    [9]  0 16  8 17  4(18) 9  2 10  5 11  1 12  6 13  3 14  7 15
    [1]  0 16  8 17  4 18  9(19) 2 10  5 11  1 12  6 13  3 14  7 15
    [2]  0 16  8 17  4 18  9 19  2(20)10  5 11  1 12  6 13  3 14  7 15
    [3]  0 16  8 17  4 18  9 19  2 20 10(21) 5 11  1 12  6 13  3 14  7 15
    [4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15
    [5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15
    [6]  0 16  8 17  4 18 19  2(24)20 10 21  5 22 11  1 12  6 13  3 14  7 15
    [7]  0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15

The goal is to be the player with the highest score after the last marble is
used up. Assuming the example above ends after the marble numbered 25, the
winning score is 23+9=32 (because player 5 kept marble 23 and removed marble
9, while no other player got any points in this very short example game).

Here are a few more examples:

  - 10 players; last marble is worth 1618 points: high score is 8317
  - 13 players; last marble is worth 7999 points: high score is 146373
  - 17 players; last marble is worth 1104 points: high score is 2764
  - 21 players; last marble is worth 6111 points: high score is 54718
  - 30 players; last marble is worth 5807 points: high score is 37305

What is the winning Elf's score?

>
> import Helpers
>
> parse :: String -> (Int, Int)
> parse s = let (p, m) = mapSnd (takeWhile (/= ' ') . drop 30) . split ' ' $ s
>            in (read p, read m)
>
> isMod23 :: Int -> Bool
> isMod23 = (== 0) . flip mod 23
>
> type GameState = ([Int], [Int])
>
> gsA :: Int -> GameState -> GameState
> gsA i (as, bs) = (as, i:bs)
>
> gsD :: GameState -> (Int, GameState)
> gsD (as, [])   = gsD ([], reverse as)
> gsD (as, b:bs) = (b, (as, bs))
>
> gsC :: Int -> GameState -> GameState
> gsC 0 gs       = gs
> gsC i ((a:as), [])
>   | i > 0     = gsC i       ([], reverse (a:as))
>   | i < 0     = gsC (i + 1) (as, [a])
> gsC i ([], (b:bs))
>   | i > 0     = gsC (i - 1) ([b], bs)
>   | i < 0     = gsC i       (reverse (b:bs), [])
> gsC i ((a:as), (b:bs))
>   | i > 0     = gsC (i - 1) (b:a:as, bs)
>   | i < 0     = gsC (i + 1) (as, a:b:bs)
>
> scores = game 1 ([], [0])
>   where
>     game i gs
>       | isMod23 i = let (j, ns) = gsD $ gsC (-7) gs
>                      in (i + j) : (game (i+1) ns)
>       | otherwise = game (i+1) (gsA i . gsC 2 $ gs)
>
> highscore i xs = let ys = xs ++ (replicate (i - (length xs) `mod` i) 0)
>                      f [] = []
>                      f as = let (z, zs) = splitAt i as
>                              in z : f zs
>                   in maximum . foldl1 (zipWith (+)) $ f ys
>
> part1 (p, s) = highscore p . take (s `div` 23) $ scores
>
> day09 = solve "09" (part1 . parse)


--- Part Two ---

Amused by the speed of your answer, the Elves are curious:

What would the new winning Elf's score be if the number of the last marble
were 100 times larger?

>
> part2 (p, s) = part1 (p, 100 * s)
>
> day09p2 = solve "09" (part2 . parse)
