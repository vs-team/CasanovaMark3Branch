namespace Expressions

Value is Expr

Data "$i" -> << int >> : Value Priority 5
Data "$f" -> << float >> : Value Priority 5
Data Expr -> "+" -> Expr : Expr Priority 1
Func "eval" -> Expr : Value Priority 0
Func "start" : int


---------------------
eval ($i x) -> ($i x)

----------------------
eval ($f x) -> ($f x)

<<x + y>> -> res
------------------
eval $i x + $i y -> $i res

main:
eval ($i 5) -> ($i x)
-----------------------
start -> x