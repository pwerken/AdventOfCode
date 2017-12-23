--- Day 20: Particle Swarm ---

Suddenly, the GPU contacts you, asking for help. Someone has asked it to
simulate too many particles, and it won't be able to finish them all in time
to render the next frame at this rate.

It transmits to you a buffer (your puzzle input) listing each particle in
order (starting with particle 0, then particle 1, particle 2, and so on).
For each particle, it provides the X, Y, and Z coordinates for the
particle's position (p), velocity (v), and acceleration (a), each in the
format <X,Y,Z>.

Each tick, all particles are updated simultaneously. A particle's properties
are updated in the following order:

  - Increase the X velocity by the X acceleration.
  - Increase the Y velocity by the Y acceleration.
  - Increase the Z velocity by the Z acceleration.
  - Increase the X position by the X velocity.
  - Increase the Y position by the Y velocity.
  - Increase the Z position by the Z velocity.

Because of seemingly tenuous rationale involving z-buffering, the GPU would
like to know which particle will stay closest to position <0,0,0> in the
long term. Measure this using the Manhattan distance, which in this
situation is simply the sum of the absolute values of a particle's X, Y, and
Z position.

For example, suppose you are only given two particles, both of which stay
entirely on the X-axis (for simplicity). Drawing the current states of
particles 0 and 1 (in that order) with an adjacent a number line and diagram
of current X positions (marked in parenthesis), the following would take
place:

    p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
    p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)

    p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
    p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)

    p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
    p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)

    p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
    p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)

At this point, particle 1 will never be closer to <0,0,0> than particle 0,
and so, in the long run, particle 0 will stay closest.

Which particle will stay closest to position <0,0,0> in the long term?

>
> import Helpers
> import Data.List
>
> data XYZ = XYZ Int Int Int
> data PVA = PVA XYZ XYZ XYZ
>
> instance (Show XYZ) where
>   show (XYZ x y z) = concat [ "<",show x,",",   show y,",",   show z,">"]
> instance (Show PVA) where
>   show (PVA p v a) = concat ["p=",show p,", v=",show v,", a=",show a]
>
> parse :: String -> [PVA]
> parse = map parsePVA . lines
>
> parsePVA :: String -> PVA
> parsePVA str = PVA (parseXYZ p) (parseXYZ v) (parseXYZ a)
>   where
>     sub3D = break ((==) '>') . tail . snd . break ((==) '<')
>     (p, xs) = sub3D str
>     (v, ys) = sub3D xs
>     (a, _)  = sub3D ys
>
> parseXYZ :: String -> XYZ
> parseXYZ s = let [x,y,z] = map read . splitOn ',' $ s
>               in XYZ x y z
>
> xyzManhattan :: XYZ -> Int
> xyzManhattan = sum . map abs . xyz2List
>
> xyz2List :: XYZ -> [Int]
> xyz2List (XYZ x y z) = [x, y, z]
>
> list2xyz :: [Int] -> XYZ
> list2xyz [x, y, z] = (XYZ x y z)
>
> xyzOrder :: XYZ -> XYZ -> Ordering
> xyzOrder a b = compare (xyzManhattan a) (xyzManhattan b)
>
> pva2List :: PVA -> [XYZ]
> pva2List (PVA p v a) = [p, v, a]
>
> list2pva :: [XYZ] -> PVA
> list2pva [p, v, a] = (PVA p v a)
>
> pvaOrderA :: PVA -> PVA -> Ordering
> pvaOrderA a b = let as = reverse . pva2List $ a
>                     bs = reverse . pva2List $ b
>                  in compareList $ zipWith xyzOrder as bs
>
> compareList [] = EQ
> compareList (x:xs)
>   | x == EQ   = compareList xs
>   | otherwise = x
>
>
> findClosest :: [PVA] -> Int
> findClosest = let f a b = pvaOrder (snd a) (snd b)
>                in fst . head . sortBy f . zipWith (,) [0..]
>
> day20 = solve "20" (findClosest . parse)


--- Part Two ---

To simplify the problem further, the GPU would like to remove any particles
that collide. Particles collide if their positions ever exactly match.
Because particles are updated simultaneously, more than two particles can
collide at the same time and place. Once particles collide, they are removed
and cannot collide with anything else after that tick.

For example:

    p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
    p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
    p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>    (0)   (1)   (2)            (3)
    p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>

    p=<-3,0,0>, v=< 3,0,0>, a=< 0,0,0>
    p=<-2,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
    p=<-1,0,0>, v=< 1,0,0>, a=< 0,0,0>             (0)(1)(2)      (3)
    p=< 2,0,0>, v=<-1,0,0>, a=< 0,0,0>

    p=< 0,0,0>, v=< 3,0,0>, a=< 0,0,0>
    p=< 0,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
    p=< 0,0,0>, v=< 1,0,0>, a=< 0,0,0>                       X (3)
    p=< 1,0,0>, v=<-1,0,0>, a=< 0,0,0>

    ------destroyed by collision------
    ------destroyed by collision------    -6 -5 -4 -3 -2 -1  0  1  2  3
    ------destroyed by collision------                      (3)
    p=< 0,0,0>, v=<-1,0,0>, a=< 0,0,0>

In this example, particles 0, 1, and 2 are simultaneously destroyed at the
time and place marked X. On the next tick, particle 3 passes through
unharmed.

How many particles are left after all collisions are resolved?

>
> p1, p2, p3, p4 :: PVA
> p1 = parsePVA "p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>"
> p2 = parsePVA "p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>"
> p3 = parsePVA "p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>"
> p4 = parsePVA "p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"
>
> pvaOrder :: PVA -> PVA -> Ordering
> pvaOrder a b = let as = pva2List $ a
>                    bs = pva2List $ b
>                 in compareList $ zipWith xyzOrder as bs
>
> addXYZ :: XYZ -> XYZ -> XYZ
> addXYZ (XYZ ax ay az) (XYZ bx by bz) = XYZ (ax+bx) (ay+by) (az+bz)
>
> step :: PVA -> PVA
> step (PVA p v a)
>   = let v2 = addXYZ v a
>         p2 = addXYZ p v2
>      in PVA p2 v2 a
>
> removeCollisions :: [PVA] -> [PVA]
> removeCollisions = concat . filter ((==) 1 . length) . groupBy collide
>
> collide :: PVA -> PVA -> Bool
> collide (PVA a _ _) (PVA b _ _) = and $ zipWith (==) (xyz2List a) (xyz2List b)
>
> tick :: [PVA] -> [PVA]
> tick = removeCollisions . map step
>
> day20p2 = solve "20" (length . head . drop 1000 . iterate tick . parse)
