﻿module ParserAST

open System
open Common
open System.IO

let systemNamespace = "__System"

let builtInTypes =
  [
    "int64"
    "int"
    "uint64"
    "uint32"
    "string"
    "double"
    "float"
    "bool"
    "unit"
  ]

type Program = 
  {
    Namespace             : string
    Imports               : List<string>
    Program               : ProgramDefinition
  }
 

and ProgramDefinition = 
  {
    Declarations          : List<Declaration>
    Rules                 : List<RuleDefinition>
    Subtyping             : List<TypeDecl*TypeDecl> //declarations, rules, subtyping mapping
  }

and OpOrder =
| Prefix
| Infix
| Suffix

and Associativity =
| Left
| Right

and TypeCall =
  {
    Variable      : Id
    Type          : TypeDecl
  }
  member this.Length = this.Type.Length

and Kind =
| KindArg of CallArg * Kind //module kinds in the format (arg : kind)
| KindType of TypeDecl //explicit types in functor declarations
| Kind //Symbol *
  static member (===) (k1 : Kind, k2 : Kind) =
    match k1,k2 with
    | KindType t1,KindType t2 -> t1 === t2
    | Kind,_
    | _,Kind -> true
    | _ -> false
  static member (=!=) (k1 : Kind, k2 : Kind) = not (k1 === k2)
  override this.ToString() =
    match this with
    | KindArg(arg,k) -> (string arg) + ":" + (string k)
    | KindType _type -> (string _type)
    | Kind -> "*"

//nested is used to distinguish type definitions such as (int -> int) -> string from int -> (int -> string)
and TypeDecl =
| Arrow of TypeDecl * TypeDecl * bool //left, right, nested
| Arg of CallArg * (TypeDecl list) //name of the type, types to construct a generic data
| External of string * Position //used for external types, like <<int>>
| Unsafe //used as type placeholder as return type of external function calls
| Zero
| FunctorCall of List<CallArg>
  member this.Length =
    match this with
    | Arrow(left,right,_) -> 1 + right.Length
    | Arg(_) -> 1
    | External _ -> 1
    | Unsafe -> 0
    | Zero -> 0
    | FunctorCall _ -> 1
  static member (=!=) (t1 : TypeDecl, t2 : TypeDecl) = not (t1 === t2)
  static member (===) (t1 : TypeDecl, t2 : TypeDecl) =
    match t1,t2 with
    | Arg(Id(id1,_),_),Arg(Id(id2,_),_) ->
        id1 = id2
    | Arrow(l1,r1,_),Arrow(l2,r2,_) ->
        if l1 =!= l2 then
          false
        else
          r1 === r2
    | External _,_
    | _,External _ 
    | Unsafe, _
    | _, Unsafe -> true
    | Zero, Zero -> true
    | _ -> false
  static member SubtypeOf (t1 : TypeDecl) (t2 : TypeDecl) (subtypeDefinitions : Map<TypeDecl,List<TypeDecl>>) =
    match t1,t2 with
    | Arg(Id(id1,p1),_),Arg(Id(id2,p2),_) ->
        let subtypesOpt = 
          let tOpt = subtypeDefinitions |> Map.tryFindKey(fun t _ -> t === t1)
          match tOpt with
          | Some k -> Some subtypeDefinitions.[k]
          | _ -> None
        match subtypesOpt with
        | Some subtypes ->
            subtypes |> List.exists(fun t -> 
                                      match t with
                                      | Arg(Id(tid,_),_) ->
                                          tid = id2
                                      | _ -> failwith "Error in subtyping list format")
        | None -> false
    | Arrow(l1,r1,_),Arrow(l2,r2,_) ->
        if (l1 <> l2 && TypeDecl.SubtypeOf l1 l2 subtypeDefinitions) |> not then
          false
        else
          TypeDecl.SubtypeOf r1 r2 subtypeDefinitions
    | _ -> false
  override this.ToString() =
    match this with
    | Arrow(t1,t2,_) ->
        "(" + t1.ToString() + "->" + t2.ToString() + ")"
    | Arg(arg,gen) -> 
        arg.ToString() + 
        (if gen.Length > 0 then
          "[" + (gen |> List.map(fun g ->  string g) |> List.reduce (fun x y -> x + "," + y)) + "]"
        else
          "")
    | External(s,_) -> "<<" + s + ">>"
    | Unsafe -> "unsafe"
    | Zero -> "zero"
    | FunctorCall(call) -> call |> List.map string |> List.reduce (fun x y -> x + " " + y)

and Declaration =
| Data of SymbolDeclaration
| Func of SymbolDeclaration
| Functor of FunctorDeclaration
| Module of ModuleDeclaration
with
  override this.ToString() =
    match this with
    | Data(d) -> d.ToString()
    | Func(f) -> f.ToString()
    | Functor(tf) -> tf.ToString()
    | Module m -> m.ToString()

and ModuleDeclaration =
  {
    Name      : Id
    Args      : List<Kind>
    Return    : Kind
    Position  : Position
    Body      : List<Declaration>
  }
with
  static member Create(name,args,ret,pos,decls) =
    {
      Name = name
      Args = args
      Return = ret
      Position = pos
      Body = decls
    }


and FunctorDeclaration =
  {
    Name            : Id
    Args            : List<Kind>
    Return          : Kind
    Priority        : int
    Associativity   : Associativity
    Position        : Position
  }
  with
    static member Create(name,args,ret,priority,associativity,pos) =
      let priority =
        match priority with
        | Some i -> i
        | None -> 0
      {
        Name = name
        Args = args
        Return = ret
        Priority = priority
        Associativity = associativity
        Position = pos
      }

and SymbolDeclaration =
  {
    Name            : Id
    FullType        : TypeDecl
    Args            : TypeDecl
    Return          : TypeDecl
    Order           : OpOrder
    LeftArity       : int
    RightArity      : int
    Priority        : int
    Position        : Position
    Associativity   : Associativity
    Premises        : List<Premise>
    Generics        : List<Id>
  }
  with
    member this.ContainsGeneric (id : Id) = this.Generics |> List.exists(fun gen -> gen.Name = id.Name)
    static member Create(name,_type,args,ret,order,priority,pos,ass,prem,gen,larity,rarity) =
      {
        Name = name
        FullType = _type
        Args = args
        Return = ret
        Order = order
        Priority = priority
        Position = pos
        Associativity = ass
        Premises = prem
        Generics = gen
        LeftArity = larity
        RightArity = rarity
      }
    override this.ToString() =
      sprintf "Name = %s\n
               FullType = %s\n
               Args = %s\n
               Return = %s\n
               Order = %A\n
               Priority = %A\n
               Position = %A\n
               Associativity = %A\n
               Premises = %A\n
               Generics = %A\n"
               (this.Name.ToString()) 
               (this.FullType.ToString()) 
               (this.Args.ToString()) 
               (this.Return.ToString())
               this.Order this.Priority this.Position this.Associativity this.Premises this.Generics

and RuleDefinition =
| Rule of Rule
| TypeRule of TypeRule

and ArithExpr =
| Add of ArithExpr * ArithExpr
| Sub of ArithExpr * ArithExpr
| Mul of ArithExpr * ArithExpr
| Div of ArithExpr * ArithExpr
| Mod of ArithExpr * ArithExpr
| Nested of ArithExpr
| Value of CallArg


and Premise =
| Emit of string * Id * Position
| Arithmetic of ArithExpr * Id * Position
| FunctionCall of Call
| FunctorCall of Call
| Bind of Id * Position * List<CallArg>
| Conditional of Conditional

and CallArg =
| Literal of Literal * Position
| Id of Id * Position
| NestedExpression of List<CallArg>
| GenericType of CallArg * List<TypeDecl>
| Lambda of LambdaConclusion * List<Premise>
| DottedPath of List<Id>
with
  override this.ToString() =
    match this with
    | Literal(l,_) -> l.ToString()
    | Id(id,_) -> id.ToString()
    | GenericType(_type,gens) -> (string _type) + "[" + (gens |> List.map string |> List.reduce(fun x y -> x + "," + y)) + "]"
    | NestedExpression(args) ->
        "(" + (args |> List.fold(fun s x -> s + x.ToString()) "") + ")"
    | DottedPath path -> path |> List.map(fun x -> (string x)) |> List.reduce (fun x y -> x + "." + y)
    | Lambda(_) -> failwith "Anonymous functions not supported yet"


and Call = List<CallArg> * List<CallArg>
and Conditional = CallArg *  Predicate * CallArg
and Conclusion = 
| ValueOutput of List<CallArg> * List<CallArg>
| TypeOutput of List<CallArg> * TypeDecl
| ModuleOutput of List<CallArg> * List<CallArg> * Program
with
  override this.ToString() =
    match this with
    | ValueOutput(left,right) -> left.ToString() + " -> " + right.ToString()
    | TypeOutput(left,_type) -> (string left) + " => " + (string _type)
    | ModuleOutput(left,id,program) -> left.ToString() + " => " + id.ToString() + "{\n\t" + program.ToString() + "\n}"

and LambdaConclusion = List<CallArg * TypeDecl> * List<CallArg>

and Rule = 
  {
    Main          : bool
    Premises      : List<Premise>
    Conclusion    : Conclusion
  }

and TypeRule =
  {
    Premises      : List<Premise>
    Conclusion    : Conclusion
  }

let symbolTableData : Map<string,SymbolDeclaration> = Map.empty
let symbolTableFunc : Map<string,SymbolDeclaration> = Map.empty
let symbolTableTypeFunc : Map<string,SymbolDeclaration> = Map.empty
let symbolTableTypeAlias : Map<string,SymbolDeclaration> = Map.empty

type SymbolContext =
  {
    DataTable             : Map<Id,SymbolDeclaration>
    FuncTable             : Map<Id,SymbolDeclaration>
    ModuleTable           : Map<Id,ModuleDeclaration>
    FunctorTable          : Map<Id,FunctorDeclaration>
    TypeFuncTable         : Map<Id,SymbolDeclaration>
    TypeAliasTable        : Map<Id,SymbolDeclaration>
    Subtyping             : Map<TypeDecl,List<TypeDecl>>
  }
  with
    member this.GetSymbol (id : Id) =
      this.DataTable |> 
      Map.tryFindKey(fun (k : Id) (s : SymbolDeclaration) -> 
                        match s.Return with
                        | Arg(sarg,_) ->
                            match sarg with
                            | Id(arg1,_) -> id = arg1
                            | _ -> false
                        | _ -> false)
    static member Empty
      with get() =
        {
          DataTable = Map.empty
          FuncTable = Map.empty
          ModuleTable = Map.empty
          FunctorTable = Map.empty
          TypeFuncTable = Map.empty 
          TypeAliasTable = Map.empty
          Subtyping = Map.empty
        }

let emptyPos = { File = "empty"; Line = 0; Col = 0}
let (!!) s = Arg(Id({ Namespace = ""; Name = s },emptyPos),[])
let (!!!) s = Arg(Id({ Namespace = systemNamespace; Name = s },emptyPos),[])
let (~~) s = Id({Namespace = ""; Name = s},emptyPos)
let (-->) t1 t2 = Arrow(t1,t2,false)
let (.|) ps c = { Main = false; Premises = ps; Conclusion = c }

