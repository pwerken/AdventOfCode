--- Day 18: Duet ---

You discover a tablet containing some strange assembly code labeled simply
"Duet". Rather than bother the sound card with it, you decide to run the
code yourself. Unfortunately, you don't see any documentation, so you're
left to figure out what the instructions mean on your own.

It seems like the assembly is meant to operate on a set of registers that
are each named with a single letter and that can each hold a single integer.
You suppose each register should start with a value of 0.

There aren't that many instructions, so it shouldn't be hard to figure out
what they do. Here's what you determine:

   - snd X plays a sound with a frequency equal to the value of X.
   - set X Y sets register X to the value of Y.
   - add X Y increases register X by the value of Y.
   - mul X Y sets register X to the result of multiplying the value contained
     in register X by the value of Y.
   - mod X Y sets register X to the remainder of dividing the value contained
     in register X by the value of Y (that is, it sets X to the result of X
     modulo Y).
   - rcv X recovers the frequency of the last sound played, but only when the
     value of X is not zero. (If it is zero, the command does nothing.)
   - jgz X Y jumps with an offset of the value of Y, but only if the value
     of X is greater than zero. (An offset of 2 skips the next instruction,
     an offset of -1 jumps to the previous instruction, and so on.)

Many of the instructions can take either a register (a single letter) or a
number. The value of a register is the integer it contains; the value of a
number is that number.

After each jump instruction, the program continues with the instruction to
which the jump jumped. After any other instruction, the program continues
with the next instruction. Continuing (or jumping) off either end of the
program terminates it.

For example:
				set a 1
				add a 2
				mul a a
				mod a 5
				snd a
				set a 0
				rcv a
				jgz a -1
				set a 1
				jgz a -2

  - The first four instructions set a to 1, add 2 to it, square it, and then
    set it to itself modulo 5, resulting in a value of 4.
  - Then, a sound with frequency 4 (the value of a) is played.
  - After that, a is set to 0, causing the subsequent rcv and jgz
    instructions to both be skipped (rcv because a is 0, and jgz because a
    is not greater than 0).
  - Finally, a is set to 1, causing the next jgz instruction to activate,
    jumping back two instructions to another jump, which jumps again to the
    rcv, which ultimately triggers the recover operation.

At the time the recover operation is executed, the frequency of the last
sound played is 4.

What is the value of the recovered frequency (the value of the most recently
played sound) the first time a rcv instruction is executed with a non-zero
value?

>
> import Helpers
> import Data.Char (ord)
>
> type State = (Int, ([(Char, Int)], [Instr]))
>
> data Val = R Char | N Int
>   deriving Show
>
> data Instr  = Snd  Char
>             | Set  Char Val
>             | Add  Char Val
>             | Mul  Char Val
>             | Mod  Char Val
>             | Rcv  Val
>             | Jgz  Val  Val
>   deriving Show
>
> parse :: String -> State
> parse = (,) 0 . (,) [] . map (parseI . words) . lines
>   where
>     parseI ["snd", x]     = Snd (head x)
>     parseI ["rcv", x]     = Rcv (parseV x)
>     parseI ["jgz", x, y]  = Jgz (parseV x) (parseV y)
>     parseI ["set", x, y]  = Set (head x)   (parseV y)
>     parseI ["add", x, y]  = Add (head x)   (parseV y)
>     parseI ["mul", x, y]  = Mul (head x)   (parseV y)
>     parseI ["mod", x, y]  = Mod (head x)   (parseV y)
>     parseV = parseV' . head
>     parseV' x | '0' <= x && x <= '9'  = N (read [x])
>               | otherwise             = R x
>
> getInstr :: State -> Instr
> getInstr (p, (_, is)) = is !! p
>
> movePtr :: State -> Int -> State
> movePtr (p, s) x = (p + x, s)
>
> getValue :: State -> Val -> Int
> getValue _  (N x) = x
> getValue rs (R c) = maybe (0::Int) id . lookup c . fst . snd $ rs
>
> setValue :: State -> (Char, Int) -> State
> setValue (p, (rs, is)) v = (p, ((v:rs), is))
>
> setAndMove :: State -> (Char, Int) -> State
> setAndMove s = flip movePtr 1 . setValue s
>
> step :: State -> State
> step s = case getInstr s of
>    (Snd x)   ->  setAndMove s $ ('!', getValue s (R x))
>    (Set c x) ->  setAndMove s $ (c, getValue s x)
>    (Add c x) ->  setAndMove s $ (c, getValue s (R c) + getValue s x)
>    (Mul c x) ->  setAndMove s $ (c, getValue s (R c) * getValue s x)
>    (Mod c x) ->  setAndMove s $ (c, getValue s (R c) `mod` getValue s x)
>    (Rcv v)   ->  if getValue s v == 0 then movePtr s 1 else s
>    (Jgz x y) ->  if getValue s x > 0
>                  then movePtr s (getValue s y)
>                  else movePtr s 1
>
> isRcv :: Instr -> Bool
> isRcv (Rcv _) = True
> isRcv _       = False
>
> rcvValue :: State -> Int
> rcvValue s =  if isRcv (getInstr s) then getValue s (R '!') else 0
>
>
> example = concat  [ "set a 1\n", "add a 2\n", "mul a a\n"
>                   , "mod a 5\n", "snd a\n", "set a 0\n"
>                   , "rcv a\n", "jgz a -1\n", "set a 1\n"
>                   , "jgz a -2\n" ]
