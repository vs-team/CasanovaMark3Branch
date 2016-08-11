module TestModule

Value is Expr

Data "A" -> int : Value
Data "B" -> string : Expr
Func "f" -> (int -> int) -> string : string
Func "g" -> int : string
Func "p" -> int -> int : int
Func "test2" -> int : string
Func "test1" -> (int -> int) : string



----------
g 0 -> "Metacompilers rule!"

------------
f g s -> s


f g "Hello world!" -> x
------------------------
test1 g -> x

p x -> r
test1 r -> r2
---------------
test2 x -> r2