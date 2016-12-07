namespace TestModule

Value is Expr

Func int -> "!" -> int : Expr
Data "$" -> int : Value
Data "%" -> int : Expr
/* Data<a,b> 'a -> "," -> 'b : Tuple
Func<a,b> "fst" -> Tuple<'a,'b> : 'a
Func<a,b> "genericTest" -> Tuple<'a,Tuple<int,float>> : 'a */
Data "dataTest" -> Value : Value
Data "$$" : Value
Func "test0" -> int : Value
Func "test1" -> Value : Expr
Func "h" -> int : Expr
Func "k" -> Value : int


-----------
h 0 -> ($ 5)

-----------
h 1 -> ($ 1)


h 0 -> x
--------------
h x -> x

