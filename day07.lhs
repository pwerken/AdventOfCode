--- Day 7: Some Assembly Required ---

This year, Santa brought little Bobby Tables a set of wires and
bitwise logic gates! Unfortunately, little Bobby is a little under the
recommended age range, and he needs help assembling the circuit.

Each wire has an identifier (some lowercase letters) and can carry a 16-bit
signal (a number from 0 to 65535). A signal is provided to each wire by a
gate, another wire, or some specific value. Each wire can only get a signal
from one source, but can provide its signal to multiple destinations. A gate
provides no signal until all of its inputs have a signal.

The included instructions booklet describes how to connect the parts
together: x AND y -> z means to connect wires x and y to an AND gate, and
then connect its output to wire z.

For example:

-   123 -> x means that the signal 123 is provided to wire x.
-   x AND y -> z means that the bitwise AND of wire x and wire y is provided
    to wire z.
-   p LSHIFT 2 -> q means that the value from wire p is left-shifted by 2
    and then provided to wire q.
-   NOT e -> f means that the bitwise complement of the value from wire e is
    provided to wire f.

Other possible gates include OR (bitwise OR) and RSHIFT (right-shift). If,
for some reason, you'd like to emulate the circuit instead, almost all
programming languages (for example, C, JavaScript, or Python) provide
operators for these gates.

For example, here is a simple circuit:

123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i

After it is run, these are the signals on the wires:

d: 72
e: 507
f: 492
g: 114
h: 65412
i: 65079
x: 123
y: 456

In little Bobby's kit's instructions booklet (provided as your puzzle
input), what signal is ultimately provided to wire a?

> import Helpers
> import Data.Bits
> import Data.List
> import Text.Read
>
> type Circuit  = [ ( String , Term ) ]
> data Term     = Value Int
>               | Wire  String
>               | Not Term
>               | And Term Term
>               | Or  Term Term
>               | Lshift Int Term
>               | Rshift Int Term
>   deriving (Eq, Show)
>
> parseInput = map (flip mapSnd wireToValue . parseLine . words) . lines
>
> parseLine [y,           "->",x] = (x, Wire y)
> parseLine ["NOT",y,     "->",x] = (x, Not (Wire y))
> parseLine [y,"AND",z,   "->",x] = (x, And (Wire y) (Wire z))
> parseLine [y,"OR", z,   "->",x] = (x, Or  (Wire y) (Wire z))
> parseLine [y,"LSHIFT",z,"->",x] = (x, Lshift (read z) (Wire y))
> parseLine [y,"RSHIFT",z,"->",x] = (x, Rshift (read z) (Wire y))
>
> wireToValue :: Term -> Term
> wireToValue (Value i)     = Value i
> wireToValue (Wire s)      = case (readMaybe s :: Maybe Int) of
>                               Nothing -> Wire s
>                               Just i  -> Value i
> wireToValue (Not x)       = Not (wireToValue x)
> wireToValue (And x y)     = And (wireToValue x) (wireToValue y)
> wireToValue (Or x y)      = Or  (wireToValue x) (wireToValue y)
> wireToValue (Lshift i x)  = Lshift i (wireToValue x)
> wireToValue (Rshift i x)  = Rshift i (wireToValue x)
>
> canSolveTerm (Value _)    = True
> canSolveTerm (Wire _)     = False
> canSolveTerm (Not x)      = canSolveTerm x
> canSolveTerm (And x y)    = canSolveTerm x && canSolveTerm y
> canSolveTerm (Or  x y)    = canSolveTerm x && canSolveTerm y
> canSolveTerm (Lshift _ x) = canSolveTerm x
> canSolveTerm (Rshift _ x) = canSolveTerm x
>
> evalTerm :: Term -> Int
> evalTerm (Value i)    = 65535 .&. i
> evalTerm (Not x)      = 65535 .&. (complement (evalTerm x))
> evalTerm (And x y)    = 65535 .&. ((evalTerm x) .&. (evalTerm y))
> evalTerm (Or  x y)    = 65535 .&. ((evalTerm x) .|. (evalTerm y))
> evalTerm (Lshift i x) = 65535 .&. (shift (evalTerm x) i)
> evalTerm (Rshift i x) = 65535 .&. (shift (evalTerm x) (0 - i))
>
> fillInTerm :: String -> Int -> Term -> Term
> fillInTerm a b (Value i)    = Value i
> fillInTerm a b (Wire x)     = if (a == x) then Value b else Wire x
> fillInTerm a b (Not x)      = Not (fillInTerm a b x)
> fillInTerm a b (And x y)    = And (fillInTerm a b x) (fillInTerm a b y)
> fillInTerm a b (Or  x y)    = Or  (fillInTerm a b x) (fillInTerm a b y)
> fillInTerm a b (Lshift i x) = Lshift i (fillInTerm a b x)
> fillInTerm a b (Rshift i x) = Rshift i (fillInTerm a b x)
>
> mapF :: [(a -> a)] -> a -> a
> mapF [] a = a
> mapF (f:fs) a = mapF fs (f a)
>
> findTermFor :: String -> Circuit -> Term
> findTermFor t ((x,y):xs) = if t == x then y else findTermFor t xs
>
> forA cs = if canSolveTerm v then evalTerm v else forA gs
>   where
>     v = findTermFor "a" cs
>     (as, bs) = partition (canSolveTerm . snd) cs
>     fs = map (uncurry fillInTerm . flip mapSnd evalTerm) as
>     gs = map (flip mapSnd (mapF fs)) bs
>
> day07 = solve "input-day07.txt" (forA . parseInput)


--- Part Two ---

Now, take the signal you got on wire a, override wire b to that signal, and
reset the other wires (including wire a). What new signal is ultimately
provided to wire a?

Although it hasn't changed, you can still get your puzzle input.

> replaceB :: Circuit -> Circuit
> replaceB [] = []
> replaceB ((x, y):cs) =
>   if x == "b"
>   then (x, Value 16076) : cs
>   else (x, y) : replaceB cs
>
> day07p2 = solve "input-day07.txt" (forA . replaceB . parseInput)
