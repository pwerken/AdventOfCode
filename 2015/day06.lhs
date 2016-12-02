--- Day 6: Probably a Fire Hazard ---

Because your neighbors keep defeating you in the holiday house decorating
contest year after year, you've decided to deploy one million lights in a
1000x1000 grid.

Furthermore, because you've been especially nice this year, Santa has mailed
you instructions on how to display the ideal lighting configuration.

Lights in your grid are numbered from 0 to 999 in each direction; the lights
at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions
include whether to turn on, turn off, or toggle various inclusive ranges
given as coordinate pairs. Each coordinate pair represents opposite corners
of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore
refers to 9 lights in a 3x3 square. The lights all start turned off.

To defeat your neighbors this year, all you have to do is set up your lights
by doing the instructions Santa sent you in order.

For example:

-   turn on 0,0 through 999,999 would turn on (or leave on) every light.
-   toggle 0,0 through 999,0 would toggle the first line of 1000 lights,
    turning off the ones that were on, and turning on the ones that were
    off.
-   turn off 499,499 through 500,500 would turn off (or leave off) the
    middle four lights.

After following the instructions, how many lights are lit?

> import Helpers
> import Data.List
>
> type Pos = (Int, Int)
> data Light = Off | On | Toggle deriving (Show, Eq)
>
> isInsideSquare :: (Pos, Pos) -> Pos -> Bool
> isInsideSquare ((a, b), (p, q)) (x, y) = a <= x && x <= q && b <= y && y <= q
>
> parsePos :: String -> Pos
> parsePos z = read ("("++z++")")
>
> parseLine [_, "on", x,_,y] = ((parsePos x, parsePos y), On)
> parseLine [_, "off",x,_,y] = ((parsePos x, parsePos y), Off)
> parseLine [_,       x,_,y] = ((parsePos x, parsePos y), Toggle)
>
> parseInput = map (parseLine . words) . lines
>
> split3 :: Int -> Int -> [a] -> ([a], ([a], [a]))
> split3 x1 x2 = mapSnd (splitAt (x2 - x1 + 1)) . splitAt x1
>
> applySquare f ys (((x1,y1),(x2,y2)),l) = as ++ ns ++ cs
>   where
>     (as, (bs, cs))  = split3 y1 y2 ys
>     ns              = map (applyLine f x1 x2 l) bs
>
> applyLine f x1 x2 l xs  = as ++ ns ++ cs
>   where
>     (as, (bs, cs))    = split3 x1 x2 xs
>     ns                = map (f l) bs
>
> boolLight :: Light -> Bool -> Bool
> boolLight On     _ = True
> boolLight Off    _ = False
> boolLight Toggle x = not x
>
> countLights = sum . map (length . filter id)
> grid = replicate 1000 . replicate 1000 $ False
> followInstr = foldl (applySquare boolLight) grid
>
> day06 = solve "input-day06.txt" ( countLights . followInstr . parseInput)


--- Part Two ---

You just finish implementing your winning light pattern when you realize you
mistranslated Santa's message from Ancient Nordic Elvish.

The light grid you bought actually has individual brightness controls; each
light can have a brightness of zero or more. The lights all start at zero.

The phrase turn on actually means that you should increase the brightness of
those lights by 1.

The phrase turn off actually means that you should decrease the brightness of
those lights by 1, to a minimum of zero.

The phrase toggle actually means that you should increase the brightness of
those lights by 2.

What is the total brightness of all lights combined after following Santa's
instructions?

For example:

-   turn on 0,0 through 0,0 would increase the total brightness by 1.
-   toggle 0,0 through 999,999 would increase the total brightness by 2000000.

> intLight :: Light -> Int -> Int
> intLight On     i = i + 1
> intLight Off    i = if i < 1 then 0 else i - 1
> intLight Toggle i = i + 2
>
> grid2 = replicate 1000 . replicate 1000 $ 0
> followInstr2 = foldl (applySquare intLight) grid2
> brightness = sum . map sum
>
> day06p2 = solve "input-day06.txt" ( brightness . followInstr2 . parseInput)
