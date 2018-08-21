namespace Expressions

Value is Expr

Data "$i" -> << int >> : Value Priority 5
Data "$f" -> << double >> : Value Priority 5
Data Expr -> "+" -> Expr : Expr Priority 1
Func "eval" -> Expr : Value Priority 0
Func "start" : << double >>


---------------------
eval ($i x) -> ($i x)

----------------------
eval ($f x) -> ($f x)

eval left -> ($i l)
eval right -> ($i r)
<< l + r >> -> res
------------------
eval left + right -> $i res

eval left -> ($f l)
eval right -> ($f r)
<< l + r >> -> res
------------------
eval left + right -> $f res

main:
eval ($f 3.5) + ($f 2.3) -> ($f x)
-----------------------
start -> x