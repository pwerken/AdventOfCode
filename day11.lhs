--- Day 11: Corporate Policy ---

Santa's previous password expired, and he needs help choosing a new one.

To help him remember his new password after the old one expires, Santa has
devised a method of coming up with a password based on the previous one.
Corporate policy dictates that passwords must be exactly eight lowercase
letters (for security reasons), so he finds his new password by incrementing
his old password string repeatedly until it is valid.

Incrementing is just like counting with numbers: xx, xy, xz, ya, yb, and so
on. Increase the rightmost letter one step; if it was z, it wraps around to
a, and repeat with the next letter to the left until one doesn't wrap
around.

Unfortunately for Santa, a new Security-Elf recently started, and he has
imposed some additional password requirements:

-   Passwords must include one increasing straight of at least three
    letters, like abc, bcd, cde, and so on, up to xyz. They cannot skip
    letters; abd doesn't count.
-   Passwords may not contain the letters i, o, or l, as these letters can
    be mistaken for other characters and are therefore confusing.
-   Passwords must contain at least two different, non-overlapping pairs of
    letters, like aa, bb, or zz.

For example:

-   hijklmmn meets the first requirement (because it contains the straight
    hij) but fails the second requirement requirement (because it contains i
    and l).
-   abbceffg meets the third requirement (because it repeats bb and ff) but
    fails the first requirement.
-   abbcegjk fails the third requirement, because it only has one double
    letter (bb).
-   The next password after abcdefgh is abcdffaa.
-   The next password after ghijklmn is ghjaabcc, because you eventually
    skip all the passwords that start with ghi..., since i is not allowed.

Given Santa's current password (your puzzle input), what should his next
password be?

Your puzzle input is vzbxkghb.

> import Data.List
> import Data.Char
>
> nextPass = map chr . reverse . up . reverse . map ord
>   where
>     up (122:ys) = 97 : up ys
>     up (x:ys)   = (x+1):ys
>     up []       = [97]
>
> hasThreeStraight :: String -> Bool
> hasThreeStraight = f . map ord
>   where
>     f (a:b:c:ds) = if a + 1 == b && b + 1 == c then True else f (b:c:ds)
>     f _          = False
>
> noIOL :: String -> Bool
> noIOL = not . or . map (flip elem "iol")
>
> subPairs :: String -> [String]
> subPairs (a:b:xs) = [a,b] : subPairs (b:xs)
> subPairs _ = []
>
> isDouble :: String -> Bool
> isDouble [a,b] = a == b
>
> hasDoublePair :: String -> Bool
> hasDoublePair = (>= 2) . length . nub . sort . filter isDouble . subPairs
>
> okPassw :: String -> Bool
> okPassw s = and $ map (\f -> f s) [hasThreeStraight, noIOL, hasDoublePair]
>
> nextValidPass = head . filter okPassw . iterate nextPass . nextPass
>
> day11 = nextValidPass "vzbxkghb"


--- Part Two ---

Santa's password expired again. What's the next one?

> day11p2 = nextValidPass day11
