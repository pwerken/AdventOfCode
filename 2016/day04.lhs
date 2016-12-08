--- Day 4: Security Through Obscurity ---

Finally, you come across an information kiosk with a list of rooms. Of course,
the list is encrypted and full of decoy data, but the instructions to decode
the list are barely hidden nearby. Better remove the decoy data first.

Each room consists of an encrypted name (lowercase letters separated by
dashes) followed by a dash, a sector ID, and a checksum in square brackets.

A room is real (not a decoy) if the checksum is the five most common letters
in the encrypted name, in order, with ties broken by alphabetization. For
example:

-   aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters
    are a (5), b (3), and then a tie between x, y, and z, which are listed
    alphabetically.
-   a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters
    are all tied (1 of each), the first five are listed alphabetically.
-   not-a-real-room-404[oarel] is a real room.
-   totally-real-room-200[decoy] is not.

Of the real rooms from the list above, the sum of their sector IDs is 1514.

What is the sum of the sector IDs of the real rooms?

> import Helpers
> import Data.Char
> import Data.List
> import Data.Maybe
>
> data Room = Room  { name :: String
>                   , sector :: Int
>                   , crc :: String }
>               deriving Show
>
> calcCRC :: String -> String
> calcCRC str = take 5 . hs . gs . sort . fs . groupBy (==) $ sort str
>   where
>     fs = map (\(x:xs) -> (1 + length xs, x))
>     gs = groupBy (\x y -> fst x == fst y)
>     hs = concat . reverse . map (map snd)
>
> validCRC :: Room -> Bool
> validCRC r = crc r == calcCRC (filter isLower $ name r)
>
> sumSectors = show . sum . map sector
>
> str2room str = Room { name = n, sector = s, crc = c }
>   where
>     n = init . takeWhile (not. isDigit) $ str
>     s = read (filter isDigit str)
>     c = reverse . takeWhile isLower . reverse $ str
>
> isChar x = isLower x || isDigit x || x == '-'
> parse = map (str2room . filter isChar) . lines
>
> day04 = solve "input-day04.txt" (sumSectors . filter validCRC .  parse)


--- Part Two ---

With all the decoy data out of the way, it's time to decrypt this list and
get moving.

The room names are encrypted by a state-of-the-art shift cipher, which is
nearly unbreakable without the right software. However, the information
kiosk designers at Easter Bunny HQ were not expecting to deal with a master
cryptographer like yourself.

To decrypt a room name, rotate each letter forward through the alphabet a
number of times equal to the room's sector ID. A becomes B, B becomes C, Z
becomes A, and so on. Dashes become spaces.

For example, the real name for qzmt-zixmtkozy-ivhz-343 is
very encrypted name.

What is the sector ID of the room where North Pole objects are stored?

> decypher r = r { name = map (\c -> fromJust $ lookup c key) $ name r }
>   where
>     xs = ['a'..'z']
>     key = (:) ('-',' ') . zip xs . drop (sector r) . cycle $ xs
>
> lookingFor rs = filter (\r -> ("northpole object storage" == name r)) rs
>
> day04p2 = solve "input-day04.txt" ( show . sector . head . lookingFor
>                     . map decypher . filter validCRC . parse)
