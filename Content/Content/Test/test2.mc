module TestModule

Data "$" -> int : Value
Func "test0" -> int : Value
Func "test1" -> (int -> int) -> int : Value

g x -> res
------------------------
test1 g x -> ($ res)