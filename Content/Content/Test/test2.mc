module TestModule

Data "$" -> int : Value
Func "g" -> int : Value
Func "test1" -> (int -> int) -> int : Value


---------
g x -> ($ 0)

--------
g x -> ($ 1)

g x -> res
------------------------
test1 g x -> res