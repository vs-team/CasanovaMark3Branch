namespace GenericTest

Data[a,b] a -> "," -> b : Tuple
Data[a] a -> "::" -> List[a] : List
Data "nil" : List
Func[a,b] "length" -> List[a] : int

---------------
length nil -> 0

length xs -> l
<< l + 1 >> -> res
------------------------
length (x :: xs) -> res

