namespace Expressions

Value is Expr

Data "$i" -> <<int>> : Value Priority 5
Data "$f" -> float : Value Priority 5
Data Value -> "+" -> Value : Expr
Func "eval" -> Expr : Value
Func "start" : int


---------------------
eval ($i x) -> ($i x)

----------------------
eval ($f x) -> ($f x)

main:
eval ($i 5) -> ($i x)
-----------------------
start -> x