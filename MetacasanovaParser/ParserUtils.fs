module ParserUtils

open Common
open ParserAST

exception ParseError of string * int * int

type TypeDeclOrName =
| Type of TypeDecl
| Name of string
with
  override this.ToString() =
    match this with
    | Type(t) -> t.ToString()
    | Name(s) -> s

let rec combineArgsAndRet args ret =
  match args with
  | Arrow(left,right) -> 
      match right with
      | Arg _
      | Generic _ ->
          Arrow(left,Arrow(right,ret))
      | _ ->
          Arrow(left,combineArgsAndRet right ret)
  | Zero
  | Arg _ -> Arrow(args,ret)
  | _ -> failwith "Invalid arguments format in combineArgsAndRet"

let opPos (parsedArgs : TypeDeclOrName list) : OpOrder =
  match parsedArgs with
  | [] -> Prefix
  | x :: xs ->
      match x with
      | Name _ -> Prefix
      | Type _ -> Infix

let rec checkDecl (parsedArgs : TypeDeclOrName list) (row : int) (column : int) : string option * TypeDecl list =
  let n,tds =
    parsedArgs |> List.fold(fun (name,decls) x ->
                              match x with
                              | Type(decl) -> (name, decl :: decls)
                              | Name(n) ->
                                match name with 
                                | None -> (Some n,decls)
                                | Some _ ->
                                    raise(ParseError("Parse Error: duplicate function or data name definition", row, column))) (None,[])
  n,tds |> List.rev

let rec buildArgType (args : TypeDecl list) =
  match args with
  | [] -> Zero
  | [x] -> x
  | x :: xs ->
      match x with
      | Zero -> failwith "Invalid arg type in buildArgType"
      | _ -> x --> (buildArgType xs)

let buildDeclarationRecord opOrder name args ret pos =
  {
    Name = name
    FullType = combineArgsAndRet args ret
    Args = args
    Return = ret
    Order = opOrder
    Priority = 0
    Position = Position.Create(pos, "missing")
    Associativity = Left
    Premises = []
  }

let processParsedArgs (parsedArgs : TypeDeclOrName list) (retType : TypeDecl) (row : int) (column : int) =
  let opOrder = opPos parsedArgs
  let Some(name),args = checkDecl parsedArgs row column
  let argType = buildArgType args
  buildDeclarationRecord opOrder {Namespace =  ""; Name = name} argType retType (row, column)