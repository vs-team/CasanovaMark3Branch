module TestModule


Func "g" -> int : int
Func "test1" -> (int -> int) -> int -> string : string


g x -> res
------------------------
test1 g x s -> s