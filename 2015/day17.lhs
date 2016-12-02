--- Day 17: No Such Thing as Too Much ---

The elves bought too much eggnog again - 150 liters this time. To fit it all
into your refrigerator, you'll need to move it into smaller containers. You
take an inventory of the capacities of the available containers.

For example, suppose you have containers of size 20, 15, 10, 5, and 5
liters.  If you need to store 25 liters, there are four ways to do it:

    15 and 10
    20 and 5 (the first 5)
    20 and 5 (the second 5)
    15, 5, and 5

Filling all containers entirely, how many different combinations of
containers can exactly fit all 150 liters of eggnog?

> import Helpers
> import Data.List
> import Data.Maybe
>
> parse :: String -> [Int]
> parse = map (read) . lines
>
> combis :: Int -> [Int] -> Maybe [[Int]]
> combis 0 _      = Just [[]]
> combis _ []     = Nothing
> combis x (c:cs) = case (compare x (sum (c:cs))) of
>                     EQ -> Just [c:cs]
>                     GT -> Nothing
>                     LT -> case (as, bs) of
>                             (Nothing, Nothing)  -> Nothing
>                             (Just x,  Nothing)  -> Just x
>                             (Nothing, Just y)   -> Just (map (c:) y)
>                             (Just x,  Just y)   -> Just (x ++ map (c:) y)
>   where
>     as = combis x cs
>     bs = combis (x-c) cs
>
> day17 = solve "input-day17.txt" (length . fromJust . combis 150 . parse)


--- Part Two ---

While playing with all the containers in the kitchen, another load of eggnog
arrives! The shipping and receiving department is requesting as many
containers as you can spare.

Find the minimum number of containers that can exactly fit all 150 liters of
eggnog. How many different ways can you fill that number of containers and
still hold exactly 150 litres?

In the example above, the minimum number of containers was two. There were
three ways to use that many containers, and so the answer there would be 3.

> freqMin = length . head . group . sort . map length
>
> day17p2 = solve "input-day17.txt" (freqMin . fromJust . combis 150 . parse)
