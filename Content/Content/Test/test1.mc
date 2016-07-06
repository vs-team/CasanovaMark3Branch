module TestModule

Func "f" -> (int -> string) -> int -> int : string
Func "sum" -> int -> int : int
Func "g" -> int : string

sum x y -> res
g res -> s
----------------
f g x y -> s