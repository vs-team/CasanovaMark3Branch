namespace Parentesization

Data int -> "$i" -> Expr : Expr Priority 1
Data "$" -> Expr -> Expr -> Expr : Expr Priority 2
Data Expr -> "%" -> Expr -> Expr : Expr Priority 3
Func "eval" -> Expr : int

------------------------------------
eval (x $i $ a1 b1 % b2 b3 a2) -> 0