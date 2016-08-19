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

let operatorRenaming =
  [
    '$', "__opDollar"
    '%', "__opMod"
    '#', "__opSharp"
    '@', "__opAt"
    '^', "__opCaret"
    '*', "__opStar"
    '?', "__opQuestionMark"
    '<', "__opLess"
    '>', "__opGreater"
    '=', "__opEqual"
    '+', "__opPlus"
    '-', "__opMinus"
    '.', "__opDot"
    '/', "_opDivision"
  ] |> Map.ofList

let renameOperator (name : string) =
  name.ToCharArray() |> 
  Array.fold(fun symbol c ->
                let replacement = operatorRenaming |> Map.tryFind c
                match replacement with
                | Some name -> symbol + name
                | None -> symbol + c.ToString()) ""