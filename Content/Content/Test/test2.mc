module TestModule

Data "$" -> int : Value
Func "test0" -> int : Value
Func "test1" -> (int -> int) -> int : Value

g 0 -> res
------------------------
test1 g 1 -> ($ res)