module TestModule 


include System.Collections.Generic
include UnityEngine

Func int -> (int -> float) -> "test" -> float => float
Data int -> "+" -> int => IntOp
Data "-" -> int => IntOp
Func "foo" -> int => string

-----------
foo x -> s