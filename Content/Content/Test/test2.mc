module TestModule


Func "g" -> int : int
Func "test1" -> (int -> int) : int

g 5 -> res
------------------------
test1 g -> res