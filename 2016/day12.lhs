--- Day 12: Leonardo's Monorail ---

You finally reach the top floor of this building: a garden with a slanted
glass ceiling. Looks like there are no more stars to be had.

While sitting on a nearby bench amidst some tiger lilies, you manage to
decrypt some of the files you extracted from the servers downstairs.

According to these documents, Easter Bunny HQ isn't just this building -
it's a collection of buildings in the nearby area. They're all connected by
a local monorail, and there's another building not far from here!
Unfortunately, being night, the monorail is currently not operating.

You remotely connect to the monorail control systems and discover that the
boot sequence expects a password. The password-checking logic (your puzzle
input) is easy to extract, but the code it uses is strange: it's assembunny
code designed for the new computer you just assembled. You'll have to
execute the code and get the password.

The assembunny code you've extracted operates on four registers (a, b, c,
and d) that start at 0 and can hold any integer. However, it seems to make
use of only a few instructions:

-   cpy x y copies x (either an integer or the value of a register) into
    register y.
-   inc x increases the value of register x by one.
-   dec x decreases the value of register x by one.
-   jnz x y jumps to an instruction y away (positive means forward; negative
    means backward), but only if x is not zero.

The jnz instruction moves relative to itself: an offset of -1 would continue
at the previous instruction, while an offset of 2 would skip over the next
instruction.

For example:

  cpy 41 a
  inc a
  inc a
  dec a
  jnz a 2
  dec a

The above code would set register a to 41, increase its value by 2, decrease
its value by 1, and then skip the last dec a (because a is not zero, so the
jnz a 2 skips it), leaving register a at 42. When you move past the last
instruction, the program halts.

After executing the assembunny code in your puzzle input, what value is left
in register a?

> import Helpers
> import Data.Char
> import Data.Maybe
>
> data Instr  = Cpy Int Char
>             | Cpr Char Char
>             | Inc Char
>             | Dec Char
>             | Jnz Char Int
>             | Jmp Int
>             | Nop
>   deriving Show
>
> type Reg =  (Char, Int)
> type Code = (Int, Instr)
>
> data Program = Program {pc :: Int, regs :: [Reg], code :: [Code]}
>   deriving Show
>
> incPC :: Int -> Program -> Program
> incPC i p = p { pc = i + pc p }
>
> getReg :: Program -> Char -> Int
> getReg p r = fromJust $ lookup r (regs p)
>
> setReg :: Program -> Char -> Int -> Program
> setReg p c i = setRegs p . ((c,i):) . filter ((/= c) . fst) $ regs p
>
> setRegs :: Program -> [Reg] -> Program
> setRegs p rs = p { regs = rs }
>
> getInstr :: Program -> Maybe Instr
> getInstr p = lookup (pc p) (code p)
>
> prog :: [Code] -> Program
> prog cs = Program {pc = 0, regs = zipWith (,) "abcd" [0,0..], code = cs}
>
> applyInstr :: Program -> Instr -> Program
> applyInstr p (Cpy y x) = incPC 1 . setReg p x                      $ y
> applyInstr p (Cpr y x) = incPC 1 . setReg p x .           getReg p $ y
> applyInstr p (Inc   x) = incPC 1 . setReg p x . ((+) 1) . getReg p $ x
> applyInstr p (Dec   x) = incPC 1 . setReg p x . ((-) 1) . getReg p $ x
> applyInstr p (Jnz x y) = incPC (if (getReg p x) == 0 then 1 else y) p
> applyInstr p (Jmp   y) = incPC y p
> applyInstr p (Nop    ) = incPC 1 p
>
> stepProgram :: Program -> Program
> stepProgram p = maybe p (applyInstr p) (getInstr p)
>
> runProgram :: Int -> Program -> Program
> runProgram i p = foldl (.) (id $!) (replicate i stepProgram) p
>
> parseInstr :: [String] -> Instr
> parseInstr ["cpy", x, [r]]
>     | all isLetter x        = Cpr (head x) r
>     | otherwise             = Cpy (read x) r
> parseInstr ["inc", [r]]     = Inc r
> parseInstr ["dec", [r]]     = Dec r
> parseInstr ["jnz", [r], x]
>     | isLetter r            = Jnz r (read x)
>     | r == '0'              = Nop
>     | otherwise             = Jmp (read x)
>
> parse :: String -> Program
> parse = prog . flip zipWith [0..] (,) . map (parseInstr . words) . lines
>
> day12 i = solve "input-day12.txt" (show . runProgram i . parse)
