--- Day 8: I Heard You Like Registers ---

You receive a signal directly from the CPU. Because of your recent
assistance with jump instructions, it would like you to compute the result
of a series of unusual register instructions.

Each instruction consists of several parts: the register to modify, whether
to increase or decrease that register's value, the amount by which to
increase or decrease it, and a condition. If the condition fails, skip the
instruction without modifying the register. The registers all start at 0.
The instructions look like this:

        b inc 5 if a > 1
        a inc 1 if b < 5
        c dec -10 if a >= 1
        c inc -20 if c == 10

These instructions would be processed as follows:

  - Because a starts at 0, it is not greater than 1, and so b is not
    modified.
  - a is increased by 1 (to 1) because b is less than 5 (it is 0).
  - c is decreased by -10 (to 10) because a is now greater than or equal
    to 1 (it is 1).
  - c is increased by -20 (to -10) because c is equal to 10.

After this process, the largest value in any register is 1.

You might also encounter <= (less than or equal to) or != (not equal to).
However, the CPU doesn't have the bandwidth to tell you what all the
registers are named, and leaves that to you to determine.

What is the largest value in any register after completing the instructions
in your puzzle input?

>
> import Helpers
> import Data.Maybe
>
> type Reg = (String, Int)
> type Action = (String, Int -> Int)
> type Condition = (String, Int -> Bool)
> type Instr = (Action, Condition)
>
> parse :: String -> [Instr]
> parse = map (parseI . words) . lines
>
> parseI :: [String] -> Instr
> parseI xs = (parseA . take 3 $ xs, parseC . drop 4 $ xs)
>
> parseA :: [String] -> Action
> parseA [v, "inc", i] = (v, (     (+) (read i)))
> parseA [v, "dec", i] = (v, (flip (-) (read i)))
>
> parseC :: [String] -> Condition
> parseC [v, "!=", i] = (v, (flip (/=) (read i)))
> parseC [v, "==", i] = (v, (flip (==) (read i)))
> parseC [v, "<=", i] = (v, (flip (<=) (read i)))
> parseC [v, ">=", i] = (v, (flip (>=) (read i)))
> parseC [v, "<",  i] = (v, (flip (<)  (read i)))
> parseC [v, ">",  i] = (v, (flip (>)  (read i)))
>
> getReg :: [Reg] -> String -> Int
> getReg rs = fromMaybe 0 . flip lookup rs
>
> checkCondition :: [Reg] -> Condition -> Bool
> checkCondition rs (v, f) = f $ getReg rs v
>
> performAction :: [Reg] -> Action -> [Reg]
> performAction rs (v, f) = (v, f $ getReg rs v) : filter ((/= v) . fst) rs
>
> doInstr :: [Reg] -> Instr -> [Reg]
> doInstr rs (a, c) = if checkCondition rs c then performAction rs a else rs
>
> execute :: [Instr] -> [[Reg]]
> execute = tail . scanl doInstr []
>
> largestResult :: [[Reg]] -> Int
> largestResult = maximum . map snd . last
>
> day08 = solve "08" (largestResult . execute . parse)


--- Part Two ---

To be safe, the CPU also needs to know the highest value held in any
register during this process so that it can decide how much memory to
allocate to these operations. For example, in the above instructions, the
highest value ever held was 10 (in register c after the third instruction
was evaluated).

>
> largestOveral :: [[Reg]] -> Int
> largestOveral = maximum . map maximum . filter (not . null) . map (map snd)
>
> day08p2 = solve "08" (largestOveral . execute . parse)
