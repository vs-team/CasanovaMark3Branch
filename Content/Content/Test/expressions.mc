namespace Expressions

Value is Expr

Data "$i" -> int : Value
Data "$f" -> float : Value
Data Value -> "+" -> Value : Expr
Func "eval" -> int -> int : int
Func "start" : int


----------------
eval 0 0 -> 0

main :
eval 0 0 -> x
eval 0 0 -> y
arithmetic x /% y -> res
-----------------------
start -> res