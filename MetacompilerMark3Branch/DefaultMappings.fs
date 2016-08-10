module DefaultMappings

open ParserAST

let operatorMappingsCSharp =
  [
    "+",(!!!"int" --> (!!!"int" --> !!!"int"),"+")
    "-",(!!!"int" --> (!!!"int" --> !!!"int"),"-")
    "*",(!!!"int" --> (!!!"int" --> !!!"int"),"*")
    "/",(!!!"int" --> (!!!"int" --> !!!"int"),"/")
  ] |> Map.ofList

let typeMappingsCsharp =
  [
    !!!"int64", "long"
    !!!"int", "int"
    !!!"uint64", "unsigned long"
    !!!"uint32", "unsigned int"
    !!!"string", "string"
    !!!"double", "double"
    !!!"float", "float"
    !!!"bool", "bool"
    !!!"unit", "__unit"
  ] |> Map.ofList