--- Day 14: Reindeer Olympics ---

This year is the Reindeer Olympics! Reindeer can fly at high speeds, but
must rest occasionally to recover their energy. Santa would like to know
which of his reindeer is fastest, and so he has them race.

Reindeer can only either be flying (always at their top speed) or resting
(not moving at all), and always spend whole seconds in either state.

For example, suppose you have the following Reindeer:

-   Comet can fly 14 km/s for 10 seconds, but then must rest for 127
    seconds.
-   Dancer can fly 16 km/s for 11 seconds, but then must rest for 162
    seconds.

After one second, Comet has gone 14 km, while Dancer has gone 16 km. After
ten seconds, Comet has gone 140 km, while Dancer has gone 160 km. On the
eleventh second, Comet begins resting (staying at 140 km), and Dancer
continues on for a total distance of 176 km. On the 12th second, both
reindeer are resting. They continue to rest until the 138th second, when
Comet flies for another ten seconds. On the 174th second, Dancer flies for
another 11 seconds.

In this example, after the 1000th second, both reindeer are resting, and
Comet is in the lead at 1120 km (poor Dancer has only gotten 1056 km by
that point). So, in this situation, Comet would win (if the race ended at
1000 seconds).

Given the descriptions of each reindeer (in your puzzle input), after
exactly 2503 seconds, what distance has the winning reindeer traveled?

>-- Prancer can fly 25 km/s for 6 seconds, but then must rest for 143 seconds.
> import Helpers
> import Data.List
>
> parseLine :: [String] -> (String, (Int, Int, Int))
> parseLine [d,_,_,x,_,_,y,_,_,_,_,_,_,z,_] = (d, (read x, read y, read z))
>
> parseInput :: String -> [(String, (Int, Int, Int))]
> parseInput = map (parseLine . words) . lines
>
> runRace :: Int -> (Int, Int, Int) -> Int
> runRace t (s, d, r)
>   | t == 0          = 0
>   | t >= (d + r)    = s*d + runRace (t - d -r) (s, d, r)
>   | t >= d          = s*d
>   | otherwise       = s*t
>
> findFartest l = maximum . map (runRace l . snd)
>
> day14 = solve "input-day14.txt" (findFartest 2503 . parseInput)


--- Part Two ---

Seeing how reindeer move in bursts, Santa decides he's not pleased with the
old scoring system.

Instead, at the end of each second, he awards one point to the reindeer
currently in the lead. (If there are multiple reindeer tied for the lead,
they each get one point.) He keeps the traditional 2503 second time limit,
of course, as doing otherwise would be entirely ridiculous.

Given the example reindeer from above, after the first second, Dancer is in
the lead and gets one point. He stays in the lead until several seconds
into Comet's second burst: after the 140th second, Comet pulls into the
lead and gets his first point. Of course, since Dancer had been in the lead
for the 139 seconds before that, he has accumulated 139 points by the 140th
second.

After the 1000th second, Dancer has accumulated 689 points, while poor
Comet, our old champion, only has 312. So, with the new scoring system,
Dancer would win (if the race ended at 1000 seconds).

Again given the descriptions of each reindeer (in your puzzle input), after
exactly 2503 seconds, how many points does the winning reindeer have?

> mapRace l = map (mapSnd (runRace l))
> distances d = map (flip mapRace d)
>
> whosAhead xs = filter ((==) m . snd) $ xs
>   where
>     m = maximum . map snd $ xs
>
> allWhosAhead x = map (map fst . whosAhead) . flip distances [1..x]
>
> mostPoints x = maximum . map length . group . sort . concat . allWhosAhead x
>
> day14p2 = solve "input-day14.txt" (mostPoints 2503 . parseInput)
