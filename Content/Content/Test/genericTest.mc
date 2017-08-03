namespace GenericTest

Func[a,b] "foo" -> List[Tuple[int,b]] -> List[Tuple[List[a],b]] : double
Data[a,b] a -> "," -> b : Tuple[a,b]
Data[a] a -> "::" -> List[a] : List[a]
Data[a] "nil" : List[a]
Func[a] "length" -> List[a] : int
Func "sum" -> List[int] : int
Func[a] List[a] -> "@" -> List[a] : List[a]
Func[a] "single" -> a : List[a]
Func[a,b] "fst" -> Tuple[a,b] : a
Func[a,b] "snd" -> Tuple[a,b] : b


-------------
sum nil -> 0

sum xs -> s
<< s + 1 >> -> res
-----------------------------
sum (x :: xs) -> res

----------------------
single x -> (x :: nil)


---------------
length nil -> 0

length xs -> l
<< l - 1 >> -> res
------------------------
length (x :: xs) -> res

---------------
fst (x,y) -> x

---------------
snd (x,y) -> y

---------------
nil @ ys -> ys

xs @ ys -> l
-----------------
(x :: xs) @ ys -> l 