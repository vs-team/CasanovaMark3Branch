namespace TestModule

Value is Expr

Func int -> "!" -> int : Expr
Data "$" -> int : Value
Data "%" -> int : Expr
Data "dataTest" -> Value : Value
Data "$$" : Value
Func "test0" -> int : Value
Func "test1" -> Value : Value
Func "h" -> int : Expr
Func "k" -> Value : int

k := ($ 5)
------------
h x -> k

----------
h 0 -> ($ 0)


h 0 -> ($ a)
------------------------
test1 (dataTest ($ 2)) -> ($ a)

------------------
k x -> 10