module TestModule

Value is Expr

Data "$" -> int : Value
Data "%" -> int : Expr
Func "test0" -> int : Value
Func "test1" -> (int -> int) -> int : Value

g 0 -> res
------------------------
test1 g 1 -> ($ res)