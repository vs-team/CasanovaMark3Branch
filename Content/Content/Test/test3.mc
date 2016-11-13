namespace TestModule

Value is Expr

Func int -> "!" -> int : Expr
Data "$" -> int : Value
Data "%" -> int : Expr
Data "dataTest" -> Value : Value
Data "$$" : Value
Func "test0" -> int : Value
Func "test1" -> Value : Expr
Func "h" -> int : Expr
Func "k" -> Value : int


-----------
h 0 -> ($ 5)


h 0 -> y
($ x) = "Hi!"
--------------
h x -> ($ x)


h 0 -> ($ x)
------------------------
test1 (dataTest ($ 2)) -> ($ 0)

