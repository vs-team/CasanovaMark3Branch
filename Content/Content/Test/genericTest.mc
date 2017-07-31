namespace GenericTest

Data[a,b] a -> "%" -> b : Tuple
Data[a] a -> "::" -> List[a] : List
Data "nil" : List
Func[a] "length" -> List[a] : int
Func[a,b] "fst" -> Tuple[a,b] : a
Func[a,b] "snd" -> Tuple[a,b] : b


---------------
length nil -> 0

length xs -> l
<< l - 1 >> -> res
------------------------
length (x :: xs) -> res

---------------
fst (x % y) -> x

---------------
snd (x % y) -> y