--- Day 8: Playground ---

Equipped with a new understanding of teleporter maintenance, you confidently
step onto the repaired teleporter pad.

You rematerialize on an unfamiliar teleporter pad and find yourself in a vast
underground space which contains a giant playground!

Across the playground, a group of Elves are working on setting up an ambitious
Christmas decoration project. Through careful rigging, they have suspended a
large number of small electrical junction boxes.

Their plan is to connect the junction boxes with long strings of lights. Most
of the junction boxes don't provide electricity; however, when two junction
boxes are connected by a string of lights, electricity can pass between those
two junction boxes.

The Elves are trying to figure out which junction boxes to connect so that
electricity can reach every junction box. They even have a list of all of the
junction boxes' positions in 3D space (your puzzle input).

For example:

    162,817,812
    57,618,57
    906,360,560
    592,479,940
    352,342,300
    466,668,158
    542,29,236
    431,825,988
    739,650,466
    52,470,668
    216,146,977
    819,987,18
    117,168,530
    805,96,715
    346,949,466
    970,615,88
    941,993,340
    862,61,35
    984,92,344
    425,690,689

This list describes the position of 20 junction boxes, one per line. Each
position is given as X,Y,Z coordinates. So, the first junction box in the list
is at X=162, Y=817, Z=812.

To save on string lights, the Elves would like to focus on connecting pairs of
junction boxes that are as close together as possible according to
straight-line distance. In this example, the two junction boxes which are
closest together are 162,817,812 and 425,690,689.

By connecting these two junction boxes together, because electricity can flow
between them, they become part of the same circuit. After connecting them,
there is a single circuit which contains two junction boxes, and the remaining
18 junction boxes remain in their own individual circuits.

Now, the two junction boxes which are closest together but aren't already
directly connected are 162,817,812 and 431,825,988. After connecting them,
since 162,817,812 is already connected to another junction box, there is now a
single circuit which contains three junction boxes and an additional 17
circuits which contain one junction box each.

The next two junction boxes to connect are 906,360,560 and 805,96,715. After
connecting them, there is a circuit containing 3 junction boxes, a circuit
containing 2 junction boxes, and 15 circuits which contain one junction box
each.

The next two junction boxes are 431,825,988 and 425,690,689. Because these two
junction boxes were already in the same circuit, nothing happens!

This process continues for a while, and the Elves are concerned that they
don't have enough extension cables for all these circuits. They would like to
know how big the circuits will be.

After making the ten shortest connections, there are 11 circuits: one circuit
which contains 5 junction boxes, one circuit which contains 4 junction boxes,
two circuits which contain 2 junction boxes each, and seven circuits which
each contain a single junction box. Multiplying together the sizes of the
three largest circuits (5, 4, and one of the circuits of size 2) produces 40.

Your list contains many junction boxes; connect together the 1000 pairs of
junction boxes which are closest together. Afterward, what do you get if you
multiply together the sizes of the three largest circuits?

>
> import Helpers
> import Data.List
>
> type Point3D = (Int, Int, Int)
> type PointPair = (Point3D, Point3D)
> type Circuit = [Point3D]
>
> parse :: String -> [Point3D]
> parse = map (p . map read . splitOn ',') . lines
>   where
>     p :: [Int] -> Point3D
>     p [x,y,z] = (x, y, z)
>
> bruteForceDists :: [Point3D] -> [(Int, PointPair)]
> bruteForceDists [x] = []
> bruteForceDists (x:xs) = allDists x xs ++ bruteForceDists xs
>
> allDists :: Point3D -> [Point3D] -> [(Int, PointPair)]
> allDists _ [] = []
> allDists p (x:ys) = (dist p x, (p, x)) : allDists p ys
>
> dist :: Point3D -> Point3D -> Int
> dist (a, b, c) (x, y, z) = (a-x)^2 + (b-y)^2 + (c-z)^2
>
> sortDists :: [(Int, PointPair)] -> [(Int, PointPair)]
> sortDists = sortBy (\x y -> compare (fst x) (fst y))
>
> shortestDists :: [Point3D] -> [PointPair]
> shortestDists = map snd . sortDists . bruteForceDists
>
> mergeCircuit ::  PointPair -> [Circuit] -> (Bool, [Circuit])
> mergeCircuit _ []  = (False, [])
> mergeCircuit (a, b) cs  = (length xs > 1, (concat xs) : ys)
>   where
>     (xs, ys) = partition (\c -> a `elem` c || b `elem` c) cs
>
> mergeCircuits :: [PointPair] -> [Circuit] -> [(Bool, [Circuit])]
> mergeCircuits _      [] = [(False, [])]
> mergeCircuits []     cs = [(False, cs)]
> mergeCircuits (p:ps) cs = let (b, ds) = mergeCircuit p cs
>                            in (b, ds) : mergeCircuits ps ds
>
> createCircuits :: [Point3D] -> [(Bool, [Circuit])]
> createCircuits ps = mergeCircuits (shortestDists ps) (map (\x -> [x]) ps)
>
> takeX :: Int -> [(Bool, [Circuit])] -> [Circuit]
> takeX x =  snd . head . drop (x - 1)
>
> longest3 :: [Circuit] -> Int
> longest3 = product . take 3 . reverse . sort . map length
>
> day08p1 = solve "08" (longest3 . takeX 1000 . createCircuits . parse)
>
> e = unlines [ "162,817,812"
>             , "57,618,57"
>             , "906,360,560"
>             , "592,479,940"
>             , "352,342,300"
>             , "466,668,158"
>             , "542,29,236"
>             , "431,825,988"
>             , "739,650,466"
>             , "52,470,668"
>             , "216,146,977"
>             , "819,987,18"
>             , "117,168,530"
>             , "805,96,715"
>             , "346,949,466"
>             , "970,615,88"
>             , "941,993,340"
>             , "862,61,35"
>             , "984,92,344"
>             , "425,690,689"
>             ]
>

--- Part Two ---

The Elves were right; they definitely don't have enough extension cables.
You'll need to keep connecting junction boxes together until they're all in
one large circuit.

Continuing the above example, the first connection which causes all of the
junction boxes to form a single circuit is between the junction boxes at
216,146,977 and 117,168,530. The Elves need to know how far those junction
boxes are from the wall so they can pick the right extension cable;
multiplying the X coordinates of those two junction boxes (216 and 117)
produces 25272.

Continue connecting the closest unconnected pairs of junction boxes together
until they're all in the same circuit. What do you get if you multiply
together the X coordinates of the last two junction boxes you need to connect?

>
> mergeAll :: [PointPair] -> [Circuit] -> PointPair
> mergeAll (p:ps) cs = let (_, ds) = mergeCircuit p cs
>                       in if length ds == 1
>                          then p
>                          else mergeAll ps ds
>
> lastPair :: [Point3D] -> PointPair
> lastPair ps = mergeAll (shortestDists ps) (map (\x -> [x]) ps)
>
> multiplyXs :: PointPair -> Int
> multiplyXs ((a, _, _), (b, _, _)) = a * b
>
> day08p2 = solve "08" (multiplyXs . lastPair . parse)
