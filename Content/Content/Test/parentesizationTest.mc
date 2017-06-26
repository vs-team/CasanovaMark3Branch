namespace Parentesization

Data int -> int -> "%" -> OpDollar -> int : OpPer     Priority 2
Data float -> OpSharp -> "$" : OpDollar     Priority 4
Data "$i" : OpSharp Priority 6
Func "eval" -> OpPer : int

-------------------------
eval (5 1 % 3.5 $ $i 4) -> 0

