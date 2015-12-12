--- Day 5: Doesn't He Have Intern-Elves For This? ---

Santa needs help figuring out which strings in his text file are naughty or
nice.

A nice string is one with all of the following properties:

-   It contains at least three vowels (aeiou only), like aei, xazegov, or
    aeiouaeiouaeiou.
-   It contains at least one letter that appears twice in a row, like xx,
    abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
-   It does not contain the strings ab, cd, pq, or xy, even if they are part
    of one of the other requirements.

For example:

-   ugknbfddgicrmopn is nice because it has at least three vowels (
    u...i...o...), a double letter (...dd...), and none of the disallowed
    substrings.
-   aaa is nice because it has at least three vowels and a double letter,
    even though the letters used by different rules overlap.
-   jchzalrnumimnmhp is naughty because it has no double letter.
-   haegwjzuvuyypxyu is naughty because it contains the string xy.
-   dvszwmarrgswjxmb is naughty because it contains only one vowel.

How many strings are nice?

> import Helpers
> import Data.List
>
> type Pos = (Int, Int)
>
> isVowel 'a' = True
> isVowel 'e' = True
> isVowel 'i' = True
> isVowel 'o' = True
> isVowel 'u' = True
> isVowel _   = False
>
> hasNiceVowels :: String -> Bool
> hasNiceVowels = (>= 3) . length . take 3 . filter isVowel
>
> subPairs :: String -> [String]
> subPairs (a:b:xs) = [a,b] : subPairs (b:xs)
> subPairs _ = []
>
> isDouble :: String -> Bool
> isDouble [a,b] = a == b
> isDouble _ = False
>
> hasDoubleSubstring :: String -> Bool
> hasDoubleSubstring = or . map isDouble . subPairs
>
> hasNaughtyCombo :: String -> Bool
> hasNaughtyCombo xs =  elem "ab" ys || elem "cd" ys ||
>                       elem "pq" ys || elem "xy" ys
>   where
>     ys = subPairs xs
>
> isNiceString :: String -> Bool
> isNiceString xs = hasNiceVowels xs &&
>                   hasDoubleSubstring xs &&
>                   not (hasNaughtyCombo xs)
>
> day05 = solve "input-day05.txt" (length . filter isNiceString . lines)


--- Part Two ---

Realizing the error of his ways, Santa has switched to a better model of
determining whether a string is naughty or nice. None of the old rules
apply, as they are all clearly ridiculous.

Now, a nice string is one with all of the following properties:

-   It contains a pair of any two letters that appears at least twice in the
    string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not
    like aaa (aa, but it overlaps).
-   It contains at least one letter which repeats with exactly one letter
    between them, like xyx, abcdefeghi (efe), or even aaa.

For example:

-   qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj)
    and a letter that repeats with exactly one letter between them (zxz).
-   xxyxx is nice because it has a pair that appears twice and a letter that
    repeats with one between, even though the letters used by each rule
    overlap.
-   uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat
    with a single letter between them.
-   ieodomkazucvgmuy is naughty because it has a repeating letter with one
    between (odo), but no pair that appears twice.

How many strings are nice under these new rules?

> isInList []  = False
> isInList [x] = False
> isInList (x:xs) = elem x (tail xs) || isInList xs
>
> containsPair = isInList . subPairs
>
> hasSkipRepeat :: String -> Bool
> hasSkipRepeat (a:b:c:xs) = (a == c) || hasSkipRepeat (b:c:xs)
> hasSkipRepeat _ = False
>
> isNiceString2 :: String -> Bool
> isNiceString2 xs = containsPair xs && hasSkipRepeat xs
>
> day05p2 = solve "input-day05.txt" (length . filter isNiceString2 . lines)
