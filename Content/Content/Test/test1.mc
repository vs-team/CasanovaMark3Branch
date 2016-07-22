module TestModule

Func "f" -> (int -> int) -> string : string
Func "g" -> int : int
Func "p" -> int -> int : int
Func "test2" -> int : string
Func "test1" -> (int -> int) : string



----------
g x -> x

------------
f g s -> s


f g "Hello world!" -> x
------------------------
test1 g -> x

p x -> r
test1 r -> r2
---------------
test2 x -> r2