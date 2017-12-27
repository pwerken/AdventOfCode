--- Day 23: Coprocessor Conflagration ---

You decide to head directly to the CPU and fix the printer from there. As
you get close, you find an experimental coprocessor doing so much work that
the local programs are afraid it will halt and catch fire. This would cause
serious issues for the rest of the computer, so you head in and see what you
can do.

The code it's running seems to be a variant of the kind you saw recently on
that tablet. The general functionality seems very similar, but some of the
instructions are different:

  - set X Y sets register X to the value of Y.
  - sub X Y decreases register X by the value of Y.
  - mul X Y sets register X to the result of multiplying the value contained
    in register X by the value of Y.
  - jnz X Y jumps with an offset of the value of Y, but only if the value of
    X is not zero. (An offset of 2 skips the next instruction, an offset of
    -1 jumps to the previous instruction, and so on.)

Only the instructions listed above are used. The eight registers here, named
a through h, all start at 0.

The coprocessor is currently set to some kind of debug mode, which allows
for testing, but prevents it from doing any meaningful work.

If you run the program (your puzzle input), how many times is the mul
instruction invoked?

>
> import Helpers
>
> type State = ([Instr], ProgramState)
>
> type Regs = [(Char, Int)]
>
> data ProgramState = ProgramState { pc :: Int, regs :: Regs }
>   deriving Show
>
> data Val = R Char | N Int
>   deriving Show
>
> data Instr  = Set  Char Val
>             | Sub  Char Val
>             | Mul  Char Val
>             | Jnz  Val  Val
>             | Hlt
>   deriving Show
>
> parse :: String -> [Instr]
> parse = (++ [Hlt]) . map (parseI . words) . lines
>   where
>     parseI ["set", x, y]  = Set (head x)   (parseV y)
>     parseI ["sub", x, y]  = Sub (head x)   (parseV y)
>     parseI ["mul", x, y]  = Mul (head x)   (parseV y)
>     parseI ["jnz", x, y]  = Jnz (parseV x) (parseV y)
>     parseV x =  if length x > 1 || ("0" <= x && x <= "9")
>                 then N (read x) else R (head x)
>
> toState :: [Instr] -> State
> toState is = (is, ProgramState 0 [])
>
> getInstr :: State -> Instr
> getInstr (is, ps) = is !! (pc ps)
>
> movePtr :: State -> Int -> State
> movePtr (is, ps) x = (is, ps { pc = x + pc ps })
>
> getValue :: State -> Val -> Int
> getValue _       (N x) = x
> getValue (_, ps) (R c) = maybe (0::Int) id . lookup c . regs $ ps
>
> setValue :: State -> (Char, Int) -> State
> setValue (is, ps) v = (is, ps { regs = rs })
>   where
>     rs = (v:) . filter (not . (==) (fst v) . fst) . regs $ ps
>
> setAndMove :: State -> (Char, Int) -> State
> setAndMove s = flip movePtr 1 . setValue s
>
> step :: State -> State
> step s = case getInstr s of
>    (Set c x) ->  setAndMove s $ (c, getValue s x)
>    (Sub c x) ->  setAndMove s $ (c, getValue s (R c) - getValue s x)
>    (Mul c x) ->  setAndMove s $ (c, getValue s (R c) * getValue s x)
>    (Jnz x y) ->  if getValue s x /= 0
>                  then movePtr s (getValue s y)
>                  else movePtr s 1
>    (Hlt)     -> s
>
> isHalt :: Instr -> Bool
> isHalt (Hlt)    = True
> isHalt _        = False
>
> isMul :: Instr -> Bool
> isMul (Mul _ _) = True
> isMul _         = False
>
> runToHalt = takeWhile (not . isHalt . getInstr) . iterate step
>
> countMul = length . filter (isMul) . map getInstr
>
> day23 = solve "23" (countMul . runToHalt . toState . parse)


--- Part Two ---

Now, it's time to fix the problem.

The debug mode switch is wired directly to register a. You flip the switch,
which makes register a now start at 1 when the program is executed.

Immediately, the coprocessor begins to overheat. Whoever wrote this program
obviously didn't choose a very efficient implementation. You'll need to
optimize the program if it has any hope of completing before Santa needs
that printer working.

The coprocessor's ultimate goal is to determine the final value left in
register h once the program completes. Technically, if it had that... it
wouldn't even need to run the program.

After setting register a to 1, if the program were to run to completion,
what value would be left in register h?

>
> getB :: State -> Int
> getB = flip getValue (R 'b') . step
>
> toRange :: Int -> [Int]
> toRange = take 1001 . iterate (+17) . (+100000) . (*100)
>
> countNonPrimes :: [Int] -> Int
> countNonPrimes = length . filter (not . isPrime)
>
> day23p2 = solve "23" (countNonPrimes . toRange . getB . toState . parse)
>
>
> isPrime :: Int -> Bool
> isPrime x = isPrime' x primes
>   where
>     isPrime' x (p:ps)
>       | x < p           = False
>       | x == p          = True
>       | x `mod` p == 0  = False
>       | otherwise       = isPrime' x ps
>
> primes :: [Int]
> primes = 2 : primes'
>   where
>     primes' = sieve [3,5..] 9 primes'
>     sieve (x:xs) q ps@ ~(p:t)
>       | x < q     = x : sieve xs q ps
>       | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t
>
> minus (x:xs) (y:ys) = case (compare x y) of
>         LT -> x : minus  xs  (y:ys)
>         EQ ->     minus  xs     ys
>         GT ->     minus (x:xs)  ys
> minus  xs     _     = xs
