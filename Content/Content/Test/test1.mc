module TestModule 


include System.Collections.Generic
include UnityEngine

Func int -> (int -> float) -> "Test" -> float => float
Data int -> "+" -> int => IntOp
Data "-" -> int => IntOp
Func "foo" -> int => string