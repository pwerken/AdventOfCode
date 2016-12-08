--- Day 8: Two-Factor Authentication ---

You come across a door implementing what you can only assume is an
implementation of two-factor authentication after a long game of
requirements telephone.

To get past the door, you first swipe a keycard (no problem; there was one
on a nearby desk). Then, it displays a code on a little screen, and you type
that code on a keypad. Then, presumably, the door unlocks.

Unfortunately, the screen has been smashed. After a few minutes, you've
taken everything apart and figured out how it works. Now you just have to
work out what the screen would have displayed.

The magnetic strip on the card you swiped encodes a series of instructions
for the screen; these instructions are your puzzle input. The screen is 50
pixels wide and 6 pixels tall, all of which start off, and is capable of
three somewhat peculiar operations:

-   rect AxB turns on all of the pixels in a rectangle at the top-left of
    the screen which is A wide and B tall.
-   rotate row y=A by B shifts all of the pixels in row A (0 is the top row)
    right by B pixels. Pixels that would fall off the right end appear at
    the left end of the row.
-   rotate column x=A by B shifts all of the pixels in column A (0 is the
    left column) down by B pixels. Pixels that would fall off the bottom
    appear at the top of the column.

For example, here is a simple sequence on a smaller screen:

-   rect 3x2 creates a small rectangle in the top-left corner:

        ###....
        ###....
        .......

-   rotate column x=1 by 1 rotates the second column down by one pixel:

        #.#....
        ###....
        .#.....

-   rotate row y=0 by 4 rotates the top row right by four pixels:

        ....#.#
        ###....
        .#.....

-   rotate column x=1 by 1 again rotates the second column down by one
    pixel, causing the bottom pixel to wrap back to the top:

        .#..#.#
        #.#....
        .#.....

As you can see, this display technology is extremely powerful, and will soon
dominate the tiny-code-displaying-screen market. That's what the
advertisement on the back of the display tries to convince you, anyway.

There seems to be an intermediate check of the voltage used by the display:
after you swipe your card, if the screen did work, how many pixels should be
lit?

> import Helpers
> import Data.List (transpose)
>
> pixels :: [[Bool]]
> pixels = replicate 6 (replicate 50 False)
>
> data Action = Rect Int Int | Row Int Int | Col Int Int
>         deriving Show
>
> doAction :: [[Bool]] -> Action -> [[Bool]]
> doAction ps     (Rect _ 0) = ps
> doAction (p:ps) (Rect w h) = (replicate w True ++ drop w p)
>                            : doAction ps (Rect w (h - 1))
> doAction (p:ps) (Row  0 y) = let z = length p
>                               in (take z . drop (z - y) . cycle $ p) : ps
> doAction (p:ps) (Row  x y) = p : doAction ps (Row (x - 1) y)
> doAction ps     (Col  x y) = transpose $ doAction (transpose ps) (Row x y)
>
> doAllActions = foldl doAction pixels
>
> countPixelsOn :: [[Bool]] -> Int
> countPixelsOn = sum . map (length . filter id)
>
> toAction :: [String] -> Action
> toAction ["rect", axb]                    = let [x,y] = splitOn 'x' axb
>                                              in Rect (read x) (read y)
> toAction ["rotate", "row",    x, "by", y] = Row (read $ drop 2 x) (read y)
> toAction ["rotate", "column", x, "by", y] = Col (read $ drop 2 x) (read y)
>
> parse = map (toAction . words) . lines
>
> day08 = solve "input-day08.txt" (show . countPixelsOn . doAllActions . parse)


--- Part Two ---

You notice that the screen is only capable of displaying capital letters; in
the font it uses, each letter is 5 pixels wide and 6 tall.

After you swipe your card, what code is the screen trying to display?


> showPixels :: [[Bool]] -> String
> showPixels = unlines . map (map f)
>   where
>     f True = '#'
>     f False = '.'
>
> day08p2 = solve "input-day08.txt" (showPixels . doAllActions . parse)
