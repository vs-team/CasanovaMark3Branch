module TestModule

Value is Expr

Data "$" -> int : Value
Data "%" -> int : Expr
Func "test0" -> int : Value
Func "test1" -> (int -> int) -> Value : Value
Func "h" -> int : Expr

h 0 -> ($ a)
g a -> res
------------------------
test1 g ($ 1) -> ($ res)