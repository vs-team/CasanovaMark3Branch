module ParserAST

open System
open Common

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

type Program = string * List<string> * ProgramDefinition

and ProgramDefinition = List<Declaration> * List<RuleDefinition> * List<TypeDecl*TypeDecl> //declarations, rules, subtyping mapping

and OpOrder =
| Prefix
| Infix

and Associativity =
| Left
| Right


//nested is used to distinguish type definitions such as (int -> int) -> string from int -> (int -> string)
and TypeDecl =
| Arrow of TypeDecl * TypeDecl * bool //left, right, nested
| Generic of Id
| Arg of CallArg
| Zero
  member this.Length =
    match this with
    | Arrow(left,right,_) -> 1 + right.Length
    | Generic(_)
    | Arg(_) -> 1
    | Zero -> 0
  static member (=!=) (t1 : TypeDecl, t2 : TypeDecl) = not (t1 === t2)
  static member (===) (t1 : TypeDecl, t2 : TypeDecl) =
    match t1,t2 with
    | Arg(Id(id1,_)),Arg(Id(id2,_)) ->
        id1 = id2
    | Arrow(l1,r1,_),Arrow(l2,r2,_) ->
        if l1 =!= l2 then
          false
        else
          r1 === r2
    | Zero, Zero -> true
    | _ -> false
  static member SubtypeOf (t1 : TypeDecl) (t2 : TypeDecl) (subtypeDefinitions : Map<TypeDecl,List<TypeDecl>>) =
    match t1,t2 with
    | Arg(Id(id1,p1)),Arg(Id(id2,p2)) ->
        let subtypesOpt = 
          let tOpt = subtypeDefinitions |> Map.tryFindKey(fun t _ -> t === t1)
          match tOpt with
          | Some k -> Some subtypeDefinitions.[k]
          | _ -> None
        match subtypesOpt with
        | Some subtypes ->
            subtypes |> List.exists(fun t -> 
                                      match t with
                                      | Arg(Id(tid,_)) ->
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
    | Generic(id) ->
        "'" + id.Name
    | Arg(arg) -> arg.ToString()
    | Zero -> "zero"

and Declaration =
| Data of SymbolDeclaration
| Func of SymbolDeclaration
| TypeFunc of SymbolDeclaration
| TypeAlias of SymbolDeclaration
with
  override this.ToString() =
    match this with
    | Data(d) -> d.ToString()
    | Func(f) -> f.ToString()
    | TypeFunc(tf) -> tf.ToString()
    | TypeAlias(ta) -> ta.ToString()

and SymbolDeclaration =
  {
    Name      : Id
    FullType  : TypeDecl
    Args      : TypeDecl
    Return    : TypeDecl
    Order     : OpOrder
    Priority  : int
    Position  : Position
    Associativity : Associativity
    Premises : List<Premise>
  }
  with
    static member Create(name,_type,args,ret,order,priority,pos,ass,prem) =
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
               Premises = %A\n" 
               (this.Name.ToString()) 
               (this.FullType.ToString()) 
               (this.Args.ToString()) 
               (this.Return.ToString()) this.Order this.Priority this.Position this.Associativity this.Premises 

and RuleDefinition =
| Rule of Rule
| TypeRule of Rule

and Premise =
| FunctionCall of Call
| Bind of Id * Position * CallArg
| Conditional of Conditional

and CallArg =
| Literal of Literal * Position
| Id of Id * Position
| NestedExpression of List<CallArg>
| Lambda of LambdaConclusion * List<Premise>
with
  override this.ToString() =
    match this with
    | Literal(l,_) -> l.ToString()
    | Id(id,_) -> id.ToString()
    | NestedExpression(args) ->
        "(" + (args |> List.fold(fun s x -> s + x.ToString()) "") + ")"
    | Lambda(_) -> failwith "Anonymous functions not supported yet"


and Call = List<CallArg> * List<CallArg>
and Conditional = CallArg *  Predicate * CallArg
and Conclusion = 
| ValueOutput of List<CallArg> * List<CallArg>
| ModuleOutput of List<CallArg> * List<CallArg> * Program

and LambdaConclusion = List<CallArg*TypeDecl> * List<CallArg>

and Rule = List<Premise> * Conclusion

let symbolTableData : Map<string,SymbolDeclaration> = Map.empty
let symbolTableFunc : Map<string,SymbolDeclaration> = Map.empty
let symbolTableTypeFunc : Map<string,SymbolDeclaration> = Map.empty
let symbolTableTypeAlias : Map<string,SymbolDeclaration> = Map.empty

type SymbolContext =
  {
    DataTable             : Map<Id,SymbolDeclaration>
    FuncTable             : Map<Id,SymbolDeclaration>    
    TypeFuncTable         : Map<Id,SymbolDeclaration>
    TypeAliasTable        : Map<Id,SymbolDeclaration>
    Subtyping             : Map<TypeDecl,List<TypeDecl>>
  }
  with
    static member Empty
      with get() =
        {
          DataTable = Map.empty
          FuncTable = Map.empty
          TypeFuncTable = Map.empty
          TypeAliasTable = Map.empty
          Subtyping = Map.empty
        }

let emptyPos = { File = "empty"; Line = 0; Col = 0}
let (!!) s = Arg(Id({ Namespace = ""; Name = s },emptyPos))
let (!!!) s = Arg(Id({ Namespace = systemNamespace; Name = s },emptyPos))
let (~~) s = Id({Namespace = ""; Name = s},emptyPos)
let (-->) t1 t2 = Arrow(t1,t2,false)
let (.|) ps c = Rule(ps,c)
