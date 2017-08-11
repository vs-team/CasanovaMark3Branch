namespace Parentesization

Data int -> int -> "%" -> OpDollar -> OpDollar : OpPer     Priority 2
Data double -> "$" -> OpSharp : OpDollar     Priority 4
Data "$i" : OpSharp Priority 6
Func "eval" -> OpPer : int

-------------------------
eval 5 1 % 3.5 $ 1.0 $ 2.5 4.0 $ $i -> 0

