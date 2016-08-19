module TestModule

Value is Expr

Data "$" -> int : Value
Data "%" -> int : Expr
Data "$$" : Value
Data "dataTest" -> Value -> Value : Value
Func "test0" -> int : Value
Func "test1" -> Value : Value
Func "h" -> int : Expr
Func "k" -> Value : int


------------
h 0 -> ($ 1)

--------
k v -> 0

h 0 -> $ a
k ($ a) -> res
------------------------
test1 (dataTest ($$) ($ 2)) -> ($ res)