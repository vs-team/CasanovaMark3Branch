﻿module TestModule

Value is Expr

Data "$" -> int : Value
Data "%" -> int : Expr
Data "dataTest" -> int -> Value : Value
Func "test0" -> int : Value
Func "test1" -> (int -> int) -> Value : Value
Func "h" -> int : Expr

h 0 -> ($ a)
g a -> res
------------------------
test1 g (dataTest 1 ($ 2)) -> ($ res)