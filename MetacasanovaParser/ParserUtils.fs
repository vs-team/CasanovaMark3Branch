﻿module ParserUtils

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
  | Arrow(left,right,nested) -> 
      match right with
      | Arg _
      | Generic _ ->
          if nested then
            Arrow(args,ret,false)
          else
            Arrow(left,Arrow(right,ret,false),false)
      | _ ->
          Arrow(left,combineArgsAndRet right ret,false)
  | Zero
  | Arg _ 
  | Generic _ -> Arrow(args,ret,false)

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

let buildDeclarationRecord opOrder name args ret pos gen =
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
    Generics = gen
  }

let processParsedArgs (parsedArgs : TypeDeclOrName list) (retType : TypeDecl) (row : int) (column : int) (gen : List<Id>) =
  let opOrder = opPos parsedArgs
  let Some(name),args = checkDecl parsedArgs row column
  let argType = buildArgType args
  buildDeclarationRecord opOrder {Namespace =  ""; Name = name} argType retType (row, column) gen

let insertNamespaceAndFileName (program : Program) (fileName : string) : Program =  
  let nameSpace,imports,parsedProgram = program
  let declarations,rules,subtypes = parsedProgram
  let rec processTypeDecl (t : TypeDecl) =
    match t with
    | Arrow(left,right,n) -> Arrow(processTypeDecl left,processTypeDecl right,n)
    | Generic(id) -> Generic({ id with Namespace = nameSpace })
    | Arg(arg) -> Arg(processArg arg)
    | Zero -> Zero

  and processSymbolDecl (decl : SymbolDeclaration) =
    {
      decl with
        Name = { decl.Name with Namespace = nameSpace }
        FullType = processTypeDecl decl.FullType
        Args = processTypeDecl decl.Args
        Return = processTypeDecl decl.Return
        Position = { decl.Position with File = fileName }
        Premises = decl.Premises |> List.map processPremise
        Generics = decl.Generics |> List.map (fun id -> { id with Namespace = nameSpace })
    }
  
  and processArg =
    fun arg ->
      match arg with
      | Literal(l,p) -> Literal(l, { p with File = fileName })
      | Id(id,p) -> 
          let nativeOpt = builtInTypes |> List.tryFind (fun x -> x = id.Name)
          match nativeOpt with
          | Some native ->
              Id({ id with Namespace = systemNamespace },{ p with File = fileName })
          | None ->
              Id({ id with Namespace = nameSpace },{ p with File = fileName })
      | NestedExpression(expr) -> NestedExpression(expr |> List.map processArg)
      | _ -> failwith "Lambdas not parsed yet"
  and processArgs left right =
    let processedLeft = left |> List.map processArg
    let processedRight = right |> List.map processArg
    processedLeft,processedRight
  and processPremise (p : Premise) =
    match p with
    | FunctionCall(left,right) ->                
        FunctionCall(processArgs left right)
    | Bind(id,pos,arg) -> Bind({ id with Namespace = nameSpace },{ pos with File = fileName },processArg arg)
    | Conditional(left,c,right) ->
        Conditional(processArg left,c,processArg right)
  let processConclusion (c : Conclusion) =
    match c with
    | ValueOutput(left,right) -> ValueOutput(processArgs left right)
    | _ -> failwith "Modules not supported yet"

  let processedDeclarations =
    declarations |> List.map (fun d -> 
                                  match d with
                                  | Data(decl) -> Data(processSymbolDecl decl)
                                  | Func(decl) -> Func(processSymbolDecl decl)
                                  | TypeFunc(decl) -> TypeFunc(processSymbolDecl decl)
                                  | TypeAlias(decl) -> TypeAlias(processSymbolDecl decl))
  let processedRules =
    rules |> List.map(fun r ->
                        match r with
                        | Rule(premises,conclusion) -> Rule(premises |> List.map processPremise,processConclusion conclusion)
                        | TypeRule(premises,conclusion) -> TypeRule(premises |> List.map processPremise,processConclusion conclusion))
  let processedSubTypes =
    subtypes |> List.map(fun (lt,rt) -> 
                    match lt,rt with
                    | Arg(leftArg),Arg(rightArg) -> Arg(processArg leftArg),Arg(processArg rightArg)
                    | _ -> failwith "Something went wrong while parsing the subtypes")
  
  nameSpace,imports,(processedDeclarations,processedRules,processedSubTypes)
        
