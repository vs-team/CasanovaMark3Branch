module TestModule

Func "f" -> (int -> int) -> string : string
Func "g" -> int : int
Func "test1" -> (int -> int) : string

f g "Hello world!" -> x
------------------------
test1 g -> x