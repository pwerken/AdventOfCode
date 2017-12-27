--- Day 24: Electromagnetic Moat ---

The CPU itself is a large, black building surrounded by a bottomless pit.
Enormous metal tubes extend outward from the side of the building at regular
intervals and descend down into the void. There's no way to cross, but you
need to get inside.

No way, of course, other than building a bridge out of the magnetic
components strewn about nearby.

Each component has two ports, one on each end. The ports come in all
different types, and only matching types can be connected. You take an
inventory of the components by their port types (your puzzle input). Each
port is identified by the number of pins it uses; more pins mean a stronger
connection for your bridge. A 3/7 component, for example, has a type-3 port
on one side, and a type-7 port on the other.

Your side of the pit is metallic; a perfect surface to connect a magnetic,
zero-pin port. Because of this, the first port you use must be of type 0. It
doesn't matter what type of port you end with; your goal is just to make the
bridge as strong as possible.

The strength of a bridge is the sum of the port types in each component. For
example, if your bridge is made of components 0/3, 3/7, and 7/4, your bridge
has a strength of 0+3 + 3+7 + 7+4 = 24.

For example, suppose you had the following components:

        0/2
        2/2
        2/3
        3/4
        3/5
        0/1
        10/1
        9/10

With them, you could make the following valid bridges:

    0/1
    0/1--10/1
    0/1--10/1--9/10
    0/2
    0/2--2/3
    0/2--2/3--3/4
    0/2--2/3--3/5
    0/2--2/2
    0/2--2/2--2/3
    0/2--2/2--2/3--3/4
    0/2--2/2--2/3--3/5

(Note how, as shown by 10/1, order of ports within a component doesn't
matter. However, you may only use each port on a component once.)

Of these bridges, the strongest one is 0/1--10/1--9/10; it has a strength of
0+1 + 1+10 + 10+9 = 31.

What is the strength of the strongest bridge you can make with the
components you have available?

>
> import Helpers
> import Data.List
>
> type Con = (Int, Int)
> data T = T Con [T] deriving Show
>
> parse :: String -> [Con]
> parse = map ((\[x, y] -> (read x, read y)) . splitOn '/') . lines
>
> fits :: Int -> [Con] -> [Con]
> fits i = filter (\(x, y) -> i == x || i == y)
>
> other :: Int -> Con -> Int
> other i (x, y)
>   | x == i    = y
>   | y == i    = x
>
> makeT :: Int -> [Con] -> [T]
> makeT i cs = map (makeT' i cs) xs
>   where
>     xs = fits i cs
>
> makeT' :: Int -> [Con] -> Con -> T
> makeT' i xs x = T x (makeT (other i x) (filter (/= x) xs))
>
> strongest :: T -> Int
> strongest (T (x, y) []) = x + y
> strongest (T (x, y) zs) = x + y + maximum (map strongest zs)
>
> day24 = solve "24" (maximum . map strongest . makeT 0 . parse)


--- Part Two ---

The bridge you've built isn't long enough; you can't jump the rest of the
way.

In the example above, there are two longest bridges:

    0/2--2/2--2/3--3/4
    0/2--2/2--2/3--3/5

Of them, the one which uses the 3/5 component is stronger; its strength is
0+2 + 2+2 + 2+3 + 3+5 = 19.

What is the strength of the longest bridge you can make? If you can make
multiple bridges of the longest length, pick the strongest one.

>
> longest :: T -> (Int, Int)
> longest (T (x, y) []) = (1,     x + y)
> longest (T (x, y) zs) = (1 + i, x + y + v)
>   where
>     (i, v) = head . reverse . sort . map longest $ zs
>
>
> day24p2 = solve "24" (snd . head . map longest . makeT 0 . parse)
