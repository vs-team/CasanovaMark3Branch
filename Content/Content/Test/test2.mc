module TestModule

Value is Expr

Data "$" -> int : Value
Data "%" -> int : Expr
Data "dataTest" -> Value : Value
Data "$$" : Value
Func "test0" -> int : Value
Func "test1" -> Value : Value
Func "h" -> int : Expr
Func "k" -> Value : int

------------
h x -> ($ 5)

----------
h 0 -> ($ 0)


h 0 -> $ a
k ($ a) -> res
------------------------
test1 (dataTest ($ 2)) -> ($ res)


------------------
k x -> 10