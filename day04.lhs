--- Day 4: The Ideal Stocking Stuffer ---

Santa needs help mining some AdventCoins (very similar to bitcoins) to use
as gifts for all the economically forward-thinking little girls and boys.

To do this, he needs to find MD5 hashes which, in hexadecimal, start with at
least five zeroes. The input to the MD5 hash is some secret key (your puzzle
input, given below) followed by a number in decimal. To mine AdventCoins,
you must find Santa the lowest positive number (no leading zeroes: 1, 2, 3,
...) that produces such a hash.

For example:

-   If your secret key is abcdef, the answer is 609043, because the MD5 hash
    of abcdef609043 starts with five zeroes (000001dbbfa...), and it is the
    lowest such number to do so.
-   If your secret key is pqrstuv, the lowest number it combines with to
    make an MD5 hash starting with five zeroes is 1048970; that is, the MD5
    hash of pqrstuv1048970 looks like 000006136ef....

Your puzzle input is yzbqklnj.

> import Helpers
> import Data.Digest.Pure.MD5 (MD5Digest, md5)
> import Data.ByteString.Lazy.Char8 (ByteString, pack)
>
> puzzleInput = "yzbqklnj"
>
> coinInput i = puzzleInput ++ (show i)
>
> coinCheck :: Int -> MD5Digest -> Bool
> coinCheck i = (== replicate i '0') . take 5 . show
>
> day04 = head . filter (coinCheck 5 . md5 . pack . coinInput) $ [1..]


--- Part Two ---

Now find one that starts with six zeroes.

> day04p2 = head . filter (coinCheck 6 . md5 . pack . coinInput) $ [1..]
>
> longList = map (\x -> (coinCheck 6 . md5 . pack . coinInput $ x, x))
> showProgress ((True,  i):xs)  = [i]
> showProgress ((False, i):xs)
>   | (mod i 10000 == 0)        = i : showProgress xs
>   | otherwise                 = showProgress xs
>
> main = do putStrLn . show . showProgress . longList $ [777800000..]
