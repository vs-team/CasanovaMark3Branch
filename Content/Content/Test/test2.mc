module TestModule

Value is Expr

Data "$" -> int : Value
Data "%" -> int : Expr
Data "dataTest" -> Value : Value
Func "test0" -> int : Value
Func "test1" -> Value : Value
Func "h" -> int : Expr
Func "k" -> Value : int

h 0 -> $ a
k ($ a) -> res
------------------------
test1 (dataTest ($ 2)) -> ($ res)