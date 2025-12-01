--- Day 14: Disk Defragmentation ---

Suddenly, a scheduled job activates the system's disk defragmenter. Were the
situation different, you might sit and watch it for a while, but today, you
just don't have that kind of time. It's soaking up valuable system resources
that are needed elsewhere, and so the only option is to help it finish its
task as soon as possible.

The disk in question consists of a 128x128 grid; each square of the grid is
either free or used. On this disk, the state of the grid is tracked by the
bits in a sequence of knot hashes.

A total of 128 knot hashes are calculated, each corresponding to a single
row in the grid; each hash contains 128 bits which correspond to individual
grid squares. Each bit of a hash indicates whether that square is free (0)
or used (1).

The hash inputs are a key string (your puzzle input), a dash, and a number
from 0 to 127 corresponding to the row. For example, if your key string were
flqrgnkx, then the first row would be given by the bits of the knot hash of
flqrgnkx-0, the second row from the bits of the knot hash of flqrgnkx-1, and
so on until the last row, flqrgnkx-127.

The output of a knot hash is traditionally represented by 32 hexadecimal
digits; each of these digits correspond to 4 bits, for a total of
4 * 32 = 128 bits. To convert to bits, turn each hexadecimal digit to its
equivalent binary value, high-bit first: 0 becomes 0000, 1 becomes 0001, e
becomes 1110, f becomes 1111, and so on; a hash that begins with a0c2017...
in hexadecimal would begin with 10100000110000100000000101110000... in
binary.

Continuing this process, the first 8 rows and columns for key flqrgnkx
appear as follows, using # to denote used squares, and . to denote free
ones:

        ##.#.#..-->
        .#.#.#.#
        ....#.#.
        #.#.##.#
        .##.#...
        ##..#..#
        .#...#..
        ##.#.##.-->
        |      |
        V      V

In this example, 8108 squares are used across the entire 128x128 grid.

Given your actual key string, how many squares are used?

> import Helpers
> import Data.Bits
> import Data.Char
> import Data.List
>
> type Code = (Int, [Int])
>
> step :: Code -> (Int, Int) -> Code
> step (x, ys) (i, j) = (r, rotN (l - x) . revN j . rotN x $ ys)
>   where
>     l = length ys
>     r = (x + i + j) `mod` l
>     revN n = uncurry (++) . mapFst reverse . splitAt n
>     rotN n = take l . drop n . cycle
>
>
> knotHash :: String -> [Int]
> knotHash = concatMap (\x -> [x `div` 16, x `mod` 16]) . xor16 . encode . prep
>   where
>     prep = concat . replicate 64 . flip (++) [17,31,73,47,23] . map ord
>     encode = snd . foldl step (0, [0..255]) . zipWith (,) [0..]
>     xor16 = map (foldl1 xor) . grp16
>     grp16 = take 16 . uncurry (:) . mapSnd grp16 . splitAt 16
>
> grid :: String -> [[Int]]
> grid = gridToBinary . map knotHash . rows
>   where
>     rows = zipWith (flip (++)) (map (('-':) . show) [0..127]) . repeat
>
> gridToBinary :: [[Int]] -> [[Int]]
> gridToBinary = map (concatMap (toBinary 8))
>   where
>     toBinary 0 _ = []
>     toBinary n i = i `div` n : toBinary (n `div` 2) (i `mod` n)
>
> countGrid = sum . map sum
>
> day14 = countGrid . grid $ "xlqgujun"


--- Part Two ---

Now, all the defragmenter needs to know is the number of regions. A region
is a group of used squares that are all adjacent, not including diagonals.
Every used square is in exactly one region: lone used squares form their own
isolated regions, while several adjacent squares all count as a single
region.

In the example above, the following nine regions are visible, each marked
with a distinct digit:

        11.2.3..-->
        .1.2.3.4
        ....5.6.
        7.8.55.9
        .88.5...
        88..5..8
        .8...8..
        88.8.88.-->
        |      |
        V      V

Of particular interest is the region marked 8; while it does not appear
contiguous in this small view, all of the squares marked 8 are connected
when considering the whole 128x128 grid. In total, in this example, 1242
regions are present.

How many regions are present given your key string?

>
> sameRegion :: [Int] -> [Int] -> Int
> sameRegion (  _:[]) (  _:[]) = 0
> sameRegion (  0:xs) (  0:ys) =     sameRegion    xs    ys
> sameRegion (0:1:xs) (1:1:ys) = 1 + sameRegion    xs    ys
> sameRegion (1:0:xs) (1:1:ys) = 2 + sameRegion (0:xs (1:ys)
> sameRegion (0:0:xs) (1:0:ys) =     sameRegion xs ys
> sameRegion (1:1:xs) (1:0:ys) = 2 + sameRegion xs ys
> sameRegion (1:1:xs) (0:0:ys) = 1 + sameRegion (0:xs) (0:ys)
> sameRegion (0:1:xs) (0:0:ys) = 1 + sameRegion (0:xs) (0:ys)
>
> almostScan :: (a -> a -> b) -> [a] -> [b]
> almostScan f (x:y:zs) = f x y : almostScan f (y:zs)
> almostScan _ _        = []
>
> -- day14p2 = length . mergeGrps . grps . gridCoord . grid $ "xlqgujun"
>
> showGrid = putStrLn . unlines . viewGrid . grid $ "xlqgujun"
>
> viewGrid :: [[Int]] -> [[Char]]
> viewGrid = map (map i2c)
>   where
>     i2c 0 = '.'
>     i2c 1 = '#'
>
> ex = "flqrgnkx"
