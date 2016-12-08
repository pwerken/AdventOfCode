--- Day 7: Internet Protocol Version 7 ---

While snooping around the local network of EBHQ, you compile a list of
IP addresses (they're IPv7, of course; IPv6 is much too limited). You'd like
to figure out which IPs support TLS (transport-layer snooping).

An IP supports TLS if it has an Autonomous Bridge Bypass Annotation, or
ABBA.  An ABBA is any four-character sequence which consists of a pair of two
different characters followed by the reverse of that pair, such as xyyx or
abba. However, the IP also must not have an ABBA within any hypernet
sequences, which are contained by square brackets.

For example:

-   abba[mnop]qrst supports TLS (abba outside square brackets).
-   abcd[bddb]xyyx does not support TLS (bddb is within square brackets,
    even though xyyx is outside square brackets).
-   aaaa[qwer]tyui does not support TLS (aaaa is invalid; the interior
    characters must be different).
-   ioxxoj[asdfgh]zxcvbn supports TLS (oxxo is outside square brackets, even
    though it's within a larger string).

How many IPs in your puzzle input support TLS?

> import Helpers
>
> hasABBA :: String -> Bool
> hasABBA (a:b:c:d:es) = a /= b && b == c && a == d || hasABBA (b:c:d:es)
> hasABBA _            = False
>
> hasTLS :: ([String], [String]) -> Bool
> hasTLS (as, bs) = any hasABBA as && not ( any hasABBA bs )
>
> grp :: [String] -> ([String], [String])
> grp []       = ([],  [])
> grp [a]      = ([a], [])
> grp (a:b:cs) = mapFst (a:) . mapSnd (b:) . grp $ cs
>
> ipv7 :: String -> [String]
> ipv7 [] = [[]]
> ipv7 ('[':xs) = [] : ipv7 xs
> ipv7 (']':xs) = [] : ipv7 xs
> ipv7 (x:xs) = let ys = ipv7 xs in (x: head ys) : tail ys
>
> parse = map (grp . ipv7) . lines
>
> day07 = solve "input-day07.txt" (show . length . filter hasTLS . parse)


--- Part Two ---

You would also like to know which IPs support SSL (super-secret listening).

An IP supports SSL if it has an Area-Broadcast Accessor, or ABA, anywhere in
the supernet sequences (outside any square bracketed sections), and a
corresponding Byte Allocation Block, or BAB, anywhere in the hypernet
sequences. An ABA is any three-character sequence which consists of the same
character twice with a different character between them, such as xyx or aba.
A corresponding BAB is the same characters but in reversed positions: yxy
and bab, respectively.

For example:

-   aba[bab]xyz supports SSL (aba outside square brackets with corresponding
    bab within square brackets).
-   xyx[xyx]xyx does not support SSL (xyx, but no corresponding yxy).
-   aaa[kek]eke supports SSL (eke in supernet with corresponding kek in
    hypernet; the aaa sequence is not related, because the interior
    character must be different).
-   zazbz[bzb]cdb supports SSL (zaz has no corresponding aza, but zbz has a
    corresponding bzb, even though zaz and zbz overlap).

How many IPs in your puzzle input support SSL?

> hasSSL :: ([String], [String]) -> Bool
> hasSSL (as, bs) = any (flip elem xs) ys
>   where
>     xs = concatMap allABAs as
>     ys = concatMap (map reverse . allABAs) bs
>
> allABAs :: String -> [String]
> allABAs (a:b:c:ds)
>     | a == c && a /= b  = [a,b] : allABAs (b:c:ds)
>     | otherwise         = allABAs (b:c:ds)
> allABAs _ = []
>
> day07p2 = solve "input-day07.txt" (show . length . filter hasSSL . parse)
