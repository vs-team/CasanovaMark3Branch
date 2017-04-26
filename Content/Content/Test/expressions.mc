namespace Expressions

Value is Expr

Data "$i" -> int : Value
Data "$f" -> float : Value
Data Value -> "+" -> Value : Expr
Func "eval" -> Expr : Value
Func "start" : int


---------------------
eval ($i x) -> ($i x)

main :
eval ($i 5) -> ($i x)
-----------------------
start -> x