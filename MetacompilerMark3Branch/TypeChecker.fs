module TypeChecker

open Common
open ParserAST
open ParserUtils

exception TypeError of string

type LocalContext =
  {
    Variables                 : Map<Id,TypeDecl * Position>
    Generics                  : Map<Id,TypeDecl option>
    GenericEquivalence        : Map<Id,List<Id>>
    NextGenericIndex          : int
  }
  with
    static member Empty =
      {
        Variables = Map.empty
        Generics = Map.empty
        GenericEquivalence = Map.empty
        NextGenericIndex = 0
      }
    member this.SetGenericEquivalence (generic1 : Id) (generic2 : Id) =
      match Map.tryFind generic1 this.GenericEquivalence,Map.tryFind generic2 this.GenericEquivalence with
      | Some l1,Some l2 ->
          { this with GenericEquivalence = this.GenericEquivalence |> Map.add generic1 (generic2 :: l1) |> Map.add generic2 (generic1 :: l2) }
      | Some l1, None ->
          { this with GenericEquivalence = this.GenericEquivalence |> Map.add generic1 (generic2 :: l1) |> Map.add generic2 [generic1]  }
      | None,Some l2 ->
          { this with GenericEquivalence = this.GenericEquivalence |> Map.add generic1 [generic2] |> Map.add generic2 (generic1 :: l2)  }
      | None,None ->
          { this with GenericEquivalence = this.GenericEquivalence |> Map.add generic1 [generic2] |> Map.add generic2 [generic1]  }
    member this.IsGeneric (generic : Id) =
      match this.Generics.TryFind(generic) with
      | Some k -> true
      | None -> false
    member this.AddGenericWithOptionalType (generic : Id) (optType : TypeDecl option) =
      if this.IsGeneric generic then
        let genericName,newGenericIndex =
          generic.Name + (string this.NextGenericIndex),this.NextGenericIndex + 1
        { 
          this with 
            Generics = this.Generics.Add({ Name = genericName; Namespace = generic.Namespace },optType)
            NextGenericIndex = newGenericIndex
        }
      else
        { this with Generics = this.Generics.Add({ Name = generic.Name; Namespace = generic.Namespace },optType) }

type TypedProgramDefinition =
  {
    Module            : string
    Declarations      : List<Declaration>
    TypedRules        : List<TypedRuleDefinition>
    SymbolTable       : SymbolContext
  }

and TypedRuleDefinition =
| TypedRule of TypedRule
| TypedTypeRule of TypedRule

and TypedRule = 
  {
    Main            : bool  
    Premises        : List<Premise>
    Conclusion      : Conclusion
    Locals          : LocalContext
    ReturnType      : TypeDecl
  }

let undefinedVarError name pos =
  raise(TypeError(sprintf "Type Error: undefined variable %s at %s" name (pos.ToString())))

let fetchDataOrFunctionSymbols (_symbolTable : SymbolContext) (args : CallArg list) =
  args |>
  List.fold (fun dataOrFunctions arg ->
                match arg with
                | Id(s,_) ->
                  match _symbolTable.FuncTable |> Map.tryFind(s) with
                  | Some x -> x :: dataOrFunctions
                  | None ->
                      match _symbolTable.DataTable |> Map.tryFind(s) with
                      | Some x -> x :: dataOrFunctions
                      | None -> dataOrFunctions
                | _ -> dataOrFunctions) [] |>
  List.sortBy (fun decl -> decl.Priority) |> List.rev

let rec splitAtElement (pred : 'a -> bool) (list : 'a list) : ('a list) * ('a list) =
  match list with
  | [] -> failwith "The provided element does not exist in the list"
  | x :: xs ->
      if pred x then
        [],xs
      else
        let left,right = splitAtElement pred xs
        (x :: left,right)

let rec parentesization (_symbolTable : SymbolContext) (operators : SymbolDeclaration list) (args : CallArg list) =
  match args with
  | [] -> []
  | [Literal(l,p)] -> [Literal (l,p)]
  | [Id(id,p)] -> [Id (id,p)]
  | [NestedExpression expr] -> parentesizeExpression _symbolTable expr
  | _ ->
      if args |> List.exists(fun arg -> 
                                match arg with
                                | Id(id,_) -> operators |> List.exists(fun op -> op.Name = id)
                                | _ -> false) then
        let minPriorityOp = operators |> List.minBy(fun op -> op.Priority)
        let operatorArg = args |> List.find(fun arg ->
                                              match arg with
                                              | Id(id,_) -> id = minPriorityOp.Name
                                              | _ -> false)
        let left,right = splitAtElement (fun x ->
                                          match x with
                                          | Id(id,_) -> id = minPriorityOp.Name
                                          | _ -> false) args
        let leftPar = parentesizeExpression _symbolTable left
        let rightPar = parentesizeExpression _symbolTable right
        let leftArgs,parentesizedleftArgs = 
          if (leftPar.Length < minPriorityOp.LeftArity) then
            [],leftPar
          else
            leftPar |> List.splitAt (leftPar.Length - minPriorityOp.LeftArity)
        let parentesizedRightArgs,rightArgs = 
          if (rightPar.Length < minPriorityOp.RightArity) then 
            rightPar,[]
          else
            rightPar |> List.splitAt minPriorityOp.RightArity
        match minPriorityOp.Order with
        | Prefix when minPriorityOp.Args.Length > 0 ->
            leftArgs @ [NestedExpression (operatorArg :: parentesizedRightArgs)] @ rightArgs
        | Suffix when minPriorityOp.Args.Length > 0 ->
            leftArgs @ ((NestedExpression (parentesizedleftArgs @ [operatorArg])) :: rightArgs)
        | Infix when minPriorityOp.Args.Length > 0 ->
            leftArgs @ [NestedExpression (parentesizedleftArgs @ [operatorArg] @ parentesizedRightArgs)] @ rightArgs
        | _ -> leftArgs @ parentesizedleftArgs @ [operatorArg] @ parentesizedRightArgs @ rightArgs
      else
        args

and parentesizeExpression (_symbolTable : SymbolContext) (args : CallArg list) =
  let operatorsOrderedByPriority = fetchDataOrFunctionSymbols _symbolTable args
  let par = parentesization _symbolTable operatorsOrderedByPriority args
  par



//extract function name from a CallArg and rearrange the term in the form: functioName arg1 arg2 ... argn. The same form data constructors
let rec normalizeDataOrFunctionCall (_symbolTable : SymbolContext) (args : List<ParserAST.CallArg>) (locals : LocalContext) : List<ParserAST.CallArg> =
  let args =
    let par = parentesizeExpression _symbolTable args
    match par with
    | [NestedExpression expr] -> expr
    | _ -> par
  let normCall =
    args |> 
    List.fold(fun (fArg,args) arg ->
                match arg with
                | Literal _ ->
                    (fArg,arg :: args)
                | Id(s,_) ->
                    let localFunctionOpt = locals.Variables |> Map.tryFind(s)
                    match localFunctionOpt with
                    | Some localFunction ->
                        match fArg with
                        | [] ->
                          (arg :: fArg,args)
                        | _ ->
                          (fArg,arg :: args )
                    | None ->
                      match fArg with
                      | [] ->
                          let funcOpt = _symbolTable.FuncTable |> Map.tryFindKey(fun name sym -> name = s)
                          let dataOpt = _symbolTable.DataTable |> Map.tryFindKey(fun name sym -> name = s)
                          match funcOpt with
                          | None ->
                              match dataOpt with
                              | Some data ->
                                  match _symbolTable.DataTable.[data].Args with
                                  | Zero ->
                                      (fArg,arg :: args)
                                  | _ ->
                                    (arg :: fArg,args)
                              | None ->
                                  (fArg,arg :: args)
                          | Some func ->
                              (arg :: fArg,args)
                      | _ ->
                          (fArg,arg :: args)
                  | Lambda(_) -> failwith "Anonymous functions not supported yet"
                  | NestedExpression (nestedArgs) ->
                      (fArg,(NestedExpression(normalizeDataOrFunctionCall _symbolTable nestedArgs locals)) :: args)) ([],[])
  let argList = snd normCall |> List.rev
  let fArg = fst normCall
  match fArg with
  | [] -> raise(TypeError("Undefined function or data constructor"))
  | _ ->
    if fArg.Length > 1 then
      failwith "Something went wrong when normalizing data or function call: more than a function name found"
    else
      (fArg.Head) :: argList


//check the consistency of single types in declarations.
//If the type is of the form (t1 -> t2) we recursively check t1 and t2.
//If the type is an argument we check that it has the correct form.
//If it is an ID we look it up in the symbol table and among the built-in types.
//If this lookup fails we throw an exception.
let rec checkType (_type : TypeDecl) (genericsInScope : List<Id>) (symbolTable : SymbolContext) : TypeDecl =
  match _type with
  | Zero -> _type
  | External _ -> _type
  | Arrow(left,right,n) ->
      let leftType = checkType left genericsInScope symbolTable
      let rightType = checkType right genericsInScope symbolTable
      Arrow(leftType,rightType,n)
  | Arg(arg,genericArgs) ->
      match arg with
      | Id(arg,pos) ->
          let typeOpt = symbolTable.GetSymbol arg
          match typeOpt with
          | Some id ->
              let checkGenericArgArity id givenGenericAmount =
                let symbolArgDecl = symbolTable.DataTable.[id]
                let correctGenericAmount = symbolArgDecl.Generics.Length
                if correctGenericAmount <> givenGenericAmount then
                  raise(TypeError(sprintf "Type Error: Invalid amount of generics, given %d, expected %d, at %A" givenGenericAmount correctGenericAmount (pos.Line,pos.Col))) 
              let givenGenericAmount = genericArgs.Length
              //the type could require generic parameters to be used. If this is the case
              //we check that the arity is correct and, in case of a specific type, that the type has been defined and possibly that its generic are used correctly.
              if givenGenericAmount > 0 then
                do checkGenericArgArity id givenGenericAmount
                
                //this recursive function checks that the generic argument of a data type requiring generic arguments are used correctly.
                //This is to check nested generic type definitions like List[Tuple[List[a],b]]         
                let rec checkGenericTypeArgument (args : TypeDecl list) : unit =
                  match args with
                  | [] -> ()
                  | arg :: args ->
                      match arg with
                      | External _ -> ()
                      | Arg(Id(id,pos),[]) ->
                          let typeOpt = symbolTable.GetSymbol id
                          match typeOpt with
                          | None ->
                              if genericsInScope |> List.exists(fun gen -> gen.Name = id.Name) |> not &&
                                  (builtInTypes |> List.tryFind(fun t -> id.Name = t)).IsNone then
                                raise(TypeError(sprintf "Type Error: Undefined type %s at %A" (arg.ToString()) (pos.Line,pos.Col)))
                              do checkGenericTypeArgument args
                          | Some _ -> do checkGenericTypeArgument args
                      | Arg(Id(id,pos),generics)->
                          let typeOpt = symbolTable.GetSymbol id
                          match typeOpt with
                          | None ->
                              raise(TypeError(sprintf "Type Error: Undefined type %s at %A" (arg.ToString()) (pos.Line,pos.Col)))
                          | Some typeId ->
                            do checkGenericArgArity typeId generics.Length
                            do checkGenericTypeArgument generics
                            do checkGenericTypeArgument args
                      | _ -> 
                        raise(TypeError(sprintf "Type Error: Undefined type %s at %A" (arg.ToString()) (pos.Line,pos.Col)))
                do checkGenericTypeArgument genericArgs
                _type
              else
                 _type
          | None ->
              //The argument could be a type name or a generic type. If the declaration contains no generic arguments then
              //we are left with the only option of the type being a built-in type. Ohterwise, the argument is indeed generic
              //and we need to check whether it is defined or not.
              if genericsInScope.Length = 0 then
                let builtInTypeOpt = builtInTypes |> List.tryFind(fun t -> arg.Name = t)
                match builtInTypeOpt with
                | Some _ -> _type
                | None ->
                raise(TypeError(sprintf "Type Error: Undefined type %s at %A" (_type.ToString()) (pos.Line,pos.Col)))
              elif genericsInScope |> List.exists(fun gen -> gen.Name = arg.Name) then
                _type
              else
                raise(TypeError(sprintf "Type Error: Undefined type %s at %A" (_type.ToString()) (pos.Line,pos.Col)))
                
      | _ -> raise(TypeError(sprintf "Type Error: You cannot use Data constructors or literals in function declarations"))
      
//scan all the declarations in the program and add them to the symbol table.
let buildSymbols (declarations : List<Declaration>) (symbols : Map<Id,SymbolDeclaration>) =
//  let check (symDecl : SymbolDeclaration) =
    
  declarations |> List.fold(fun sym decl ->
                              match decl with
                              | Data(data) ->
                                  //do checkType data.Args sym |> ignore
                                  match data.Return with
                                  | Arg(Id(arg,_),_) ->
                                    {sym with DataTable = sym.DataTable.Add(data.Name,data)}
                                  | _ -> raise(TypeError(sprintf "Type Error: invalid type %s for the data %s" (data.Return.ToString()) data.Name.Name))
                              | Func(func) ->
                                  //do checkType func.Args sym |> ignore
                                  {sym with FuncTable = sym.FuncTable.Add(func.Name,func)}
                              | TypeFunc(tf) ->
                                  {sym with TypeFuncTable = sym.TypeFuncTable.Add(tf.Name,tf)}
                              | TypeAlias(ta) ->
                                  {sym with TypeAliasTable = sym.TypeAliasTable.Add(ta.Name,ta)}) SymbolContext.Empty


//check that the type of the declarations are consistent, i.e. that the type used are defined.
let checkSymbols (declarations : List<Declaration>) (symbolTable : SymbolContext) =
  for decl in declarations do
    match decl with
    | Data(data) ->
        do checkType data.Args data.Generics symbolTable |> ignore
    | Func(func) ->
        do checkType func.Args func.Generics symbolTable |> ignore
    | TypeFunc(tf) ->
        failwith "TypeFunctions not implemented yet..."
    | TypeAlias(ta) ->
        failwith "TypeAliases not implemented yet..."

let areTypesEquivalent t1 t2 ctxt = 
  t1 === t2 || (TypeDecl.SubtypeOf t1 t2 ctxt.Subtyping)

let checkTypeWithErrorMsg t1 t2 p (ctxt : SymbolContext) (locals : LocalContext) msg =
  if areTypesEquivalent t1 t2 ctxt then
    ()
  else
    raise(TypeError(msg))
     
let checkTypeEquivalence (t1: TypeDecl) (t2 : TypeDecl) (p : Position) (ctxt : SymbolContext) (locals : LocalContext)  =
  checkTypeWithErrorMsg t1 t2 p ctxt locals (sprintf "Type Error: given %s but expected %s at %s" (t1.ToString()) (t2.ToString()) (p.ToString()))

//note that this function should be used when t1 is a generic type or a data structure requiring generic parameters
let rec checkGenericTypeEquivalence (t1 : TypeDecl) (t2 : TypeDecl) (p : Position) (ctxt : SymbolContext) (locals : LocalContext) =
  let checkGenericWithNonGeneric generic nonGeneric (genericId : Id) ctxt =
    match generic with
    | Some genericType ->
        if areTypesEquivalent genericType nonGeneric ctxt then
          locals
        else
          raise(TypeError(sprintf "Type Error: The generic variable %s has been bound to type %s but %s was expected at %s" genericId.Name (string genericType) (string t2) (string p)))
    | None ->
        let newGenerics = locals.Generics.Add(genericId,Some nonGeneric)
        { locals with Generics = newGenerics }
  match t1,t2 with
  | Arg(Id(id1,_),[]),Arg(Id(id2,_),[]) ->
      match locals.Generics |> Map.tryFind id1 ,locals.Generics |> Map.tryFind id2 with
      //both types are generic
      //- if both generics have already been bound to a specific type we must check their equivalence
      //- if t2 has been bound to a specic type and t1 has not then, we assign the type of t2 to t1 (also the other way around)
      //- if t1 and t2 are unbound generics then they are simply added to the equivalence table.
      | Some gen1,Some gen2 ->
          match gen1,gen2 with
          | Some genericType1,Some genericType2 ->
              if areTypesEquivalent genericType1 genericType2 ctxt then
                locals
              else
                raise(TypeError(sprintf "Type Error: Generic variable %s has been bound to type %s but %s was given at %s" id2.Name (string genericType2) (string genericType1) (string p)))
          | Some genericType1,None ->
              let newGenerics = locals.Generics.Add(id2, Some genericType1)
              { locals with Generics = newGenerics }
          | None,Some genericType2 ->
              let newGenerics = locals.Generics.Add(id1, Some genericType2)
              { locals with Generics = newGenerics }
          | None,None ->
              locals.SetGenericEquivalence id1 id2
      //t1 is generic and t2 is specific
      //- if t1 has already been bound to a specific type check the equivalence of the types.
      //- if t1 has no specific type then set the specific type of t1 to be the one of t2.
      | Some gen1, None ->
          checkGenericWithNonGeneric gen1 t2 id1 ctxt
      //t1 is specific and t2 is generic. Same as above             
      | None,Some gen2 ->
          checkGenericWithNonGeneric gen2 t1 id2 ctxt
      //t1 and t2 are specific. Use the normal function
      | None,None ->
          do checkTypeEquivalence t1 t2 p ctxt locals
          locals
  //t1 and t2 are data types requiring generic arguments
  //the two types are equivalent if data types are equivalent and
  //if all generic arguments are equivalent
  | Arg(Id(id1,_),genericArgs1),Arg(Id(id2,_),genericArgs2) ->
      do checkTypeEquivalence t1 t2 p ctxt locals
      List.fold2(fun newLocals arg1 arg2 ->
                   match arg1,arg2 with
                   | Arg(Id(_,p1),_),Arg(_,_) ->
                     checkGenericTypeEquivalence arg1 arg2 p1 ctxt newLocals
                   | _ -> failwith "Something went wrong with the parser: generic arguments are not variables" ) locals genericArgs1 genericArgs2
  | _ -> failwith "Something went wrong: the type definition has an invalid structure"

let getLocalType id locals p =
  let idOpt = locals.Variables |> Map.tryFind id
  match idOpt with
  | Some (t,_) ->
      t             
  | None ->
      undefinedVarError id.Name p

let checkTypeDecl t1 t2 p ctxt locals =
  do checkTypeEquivalence t1 t2 p ctxt locals
  locals
    
let checkLiteral (l : Literal) (typeDecl : TypeDecl) (p : Position) (ctxt : SymbolContext) (locals : LocalContext) : TypeDecl * LocalContext =
    match l with
    | I64(_) ->
      !!!"int64", checkTypeDecl !!!"int64" typeDecl p ctxt locals
    | I32(_) ->
      !!!"int", checkTypeDecl !!!"int" typeDecl p ctxt locals
    | U64(_) ->
      !!!"uint64", checkTypeDecl !!!"uint64" typeDecl p ctxt locals
    | U32(_) ->
      !!!"uint32", checkTypeDecl !!!"uint32" typeDecl p ctxt locals
    | F64(_) ->
      !!!"double", checkTypeDecl !!!"double" typeDecl p ctxt locals
    | F32(_) ->
      !!!"float", checkTypeDecl !!!"float" typeDecl p ctxt locals
    | String(_) ->
      !!!"string", checkTypeDecl !!!"string" typeDecl p ctxt locals
    | Bool(_) ->
      !!!"bool", checkTypeDecl !!!"bool" typeDecl p ctxt locals
    | Unit ->
      !!!"unit", checkTypeDecl !!!"unit" typeDecl p ctxt locals

let getLiteralType l =
  match l with
  | I64(_) ->
    !!!"int64"
  | I32(_) ->
    !!!"int"
  | U64(_) ->
    !!!"uint64"
  | U32(_) ->
    !!!"uint32"
  | F64(_) ->
    !!!"double"
  | F32(_) ->
    !!!"float"
  | String(_) ->
    !!!"string"
  | Bool(_) ->
    !!!"bool"
  | Unit ->
    !!!"unit"


let rec checkSingleArg
  (arg : ParserAST.CallArg)
  (symbolTable : SymbolContext)
  (typeDecl : TypeDecl)
  (ctxt : LocalContext)
  (buildLocals : bool) : TypeDecl * LocalContext =

  match arg with
  | Literal(l,p) ->
      match typeDecl with
      | Arg(Id(id,_),[]) ->
              if ctxt.IsGeneric id then
                  //if the expected type is generic we must bind the generic variable to the literal type
                let literalType = getLiteralType l
                literalType,ctxt.AddGenericWithOptionalType id (Some literalType)
              else  
                checkLiteral l typeDecl p symbolTable ctxt
      | Arg(Id(id,_),_) ->
          //a literal is never compatible with a type requiring generic arguments.
          raise(TypeError(sprintf "Type Error: Given literal but expected data constructor or function accepting generics arguments at line %d column %d" p.Line p.Col))
      | External _ -> typeDecl,ctxt
      | _ ->
          failwith "Something went wrong: the type definition has an invalid structure"
  | Id(id,p) ->
      let typeNoArgsOpt = symbolTable.DataTable |> Map.tryFindKey(fun k _ -> k.Name = id.Name)
      match typeNoArgsOpt with
      | None ->
        if buildLocals then
          Arg(Id(id,p),[]),{ctxt with Variables = ctxt.Variables |> Map.add id (typeDecl,p)}
        else
          let t = getLocalType id ctxt p
          do checkTypeEquivalence t typeDecl p symbolTable ctxt
          t,ctxt
      //data with no arguments
      | Some decl ->
        let t = symbolTable.DataTable.[decl].Return
        do checkTypeEquivalence t typeDecl p symbolTable ctxt
        t,ctxt   
  | Lambda(_) -> failwith "Anonymous functions not supported yet"   
  | NestedExpression(call) ->
      let nestedType,nestedCtxt = checkNormalizedCall call symbolTable ctxt buildLocals
      match call.Head with
      | Id(id,p) ->
          let dataOpt = symbolTable.DataTable |> Map.tryFind(id)
          match dataOpt with
          | Some decl ->
              checkTypeEquivalence nestedType typeDecl p symbolTable ctxt
              nestedType,nestedCtxt
          | None -> 
              let funcOpt = symbolTable.FuncTable |> Map.tryFind(id)
              match funcOpt with
              | Some decl ->
                checkTypeEquivalence nestedType typeDecl p symbolTable ctxt
                nestedType,nestedCtxt
              | None ->
                  failwith "Something went wrong: apparently the term is neither a data constructor nor a function call"
      | _ ->
          failwith "Something went wrong when checking the nested expression"

and checkNormalizedArgs 
  (args : List<ParserAST.CallArg>)
  (symbolTable : SymbolContext)
  (typeDecl : TypeDecl)
  (ctxt : LocalContext)
  (buildLocals : bool) : TypeDecl * LocalContext =

  match args with
  | [] ->
    typeDecl,ctxt
  | x :: xs ->
      match typeDecl with
      | Arrow(left,right,_) ->
          let t,newCtxt = checkSingleArg x symbolTable left ctxt buildLocals
          checkNormalizedArgs xs symbolTable right newCtxt buildLocals
      | Zero -> raise(TypeError("Type Error: the function expects no arguments"))
      | _ -> checkSingleArg x symbolTable typeDecl ctxt buildLocals


and checkNormalizedCall 
  (call : List<ParserAST.CallArg>) 
  (symbolTable : SymbolContext) 
  (ctxt : LocalContext)
  (buildLocals : bool) : TypeDecl * LocalContext =

  let checkArgsWithCorrectCardinality args (decl : SymbolDeclaration) =
    if call.Length > decl.FullType.Length then
      raise(TypeError(sprintf "Type Error: too many arguments passed to %s" decl.Name.Name)) 
    elif call.Length = 1 then
      decl.Return,ctxt
    else
      let generics = decl.Generics |> List.map(fun gen -> (gen,None)) |> Map.ofList
      checkNormalizedArgs args symbolTable decl.FullType { ctxt with Generics = generics } buildLocals

  match call with
  | arg :: args ->
      match arg with
      | Id(id,pos) ->
        let funcOpt = symbolTable.FuncTable |> Map.tryFind(id)
        let dataOpt = symbolTable.DataTable |> Map.tryFind(id)
        let localOpt =
          if not buildLocals then
            ctxt.Variables |> Map.tryFind(id)
          else
            None
        match localOpt with
        | Some local ->
            let localSym =
              {
                Name = id
                FullType = fst local
                Args = fst local
                Return = fst local
                Order = Prefix
                Priority  = 0
                Position = pos
                Associativity = Left
                Premises = []
                Generics = []
                LeftArity = 0
                RightArity = 0
              }
            checkArgsWithCorrectCardinality args localSym
        | None ->
          match funcOpt with
          | None ->
              match dataOpt with
              | None ->
                  failwith "You are checking arguments that are not data constructors or functions with checkNormalizedCall"
              | Some dSym ->
                match dSym.Args with
                | Zero -> dSym.Return,ctxt
                | _ ->
                  checkArgsWithCorrectCardinality args dSym
          | Some fSym ->
              checkArgsWithCorrectCardinality args fSym
      | _ ->
          failwith "Something went wrong when normalizing the function call in the typechecker. The first argument is not a function name"
  | [] -> failwith "Something went wrong with the call normalization: there are no arguments in the call"

//We need to add the position to premise calls and conclusion calls
and checkPremise (premise : Premise) (symbolTable : SymbolContext) (locals : LocalContext) =
  let rec checkFunctionCallResult (result : CallArg list) (funcType : TypeDecl) =
    match result with
    | [r] -> 
          let _,newLocals = checkSingleArg r symbolTable funcType locals true
          newLocals,[r] 
      | id :: ids ->
          let normalizedData = normalizeDataOrFunctionCall symbolTable result locals
//          let normIds = normalizedData |> List.map(fun x -> match x with
//                                                            | Id(id,_) -> id
//                                                            | _ -> failwith "Invalid premise result format")
          match normalizedData.Head with
          | Id(name,pos) ->           
            let funcOpt = symbolTable.FuncTable |> Map.tryFind (name)
            match funcOpt with
            | Some _ -> raise(TypeError(sprintf "Type Error: It is not allowed to call a function in the return part of a premise at %s" (pos.ToString())))
            | None ->
                let dataType,newLocals = checkNormalizedCall normalizedData symbolTable locals true
                do checkTypeEquivalence dataType funcType Position.Zero symbolTable locals
                newLocals,normalizedData
          | _ -> failwith "Something went wrong with the function normalizer: the first element is not an id"
      | _ -> failwith "Something went wrong: the return argument of a premise is empty"

  let getLiteralType l =
    match l with
    | I64 _ -> !!!"int64"
    | U64 _ -> !!!"uint64"
    | I32 _ -> !!!"int"
    | U32 _ -> !!!"uint32"
    | F64 _ -> !!!"double"
    | F32 _ -> !!!"float"
    | String _ -> !!!"string"
    | Bool _ -> !!!"bool"
    | Unit -> !!!"unit"
    

  let rec checkArithmeticExpr (expr : ArithExpr) (position : Position) : TypeDecl =
    match expr with
    | Add(left,right)
    | Sub(left,right)
    | Mul(left,right)
    | Div(left,right)
    | Mod(left,right) ->
        let t1 = checkArithmeticExpr left position
        let t2 = checkArithmeticExpr right position
        if (t1 === !!!"int64" && t2 === !!!"int64") ||
           (t1 === !!!"uint64" && t2 === !!!"uint64") ||
           (t1 === !!!"int" && t2 === !!!"int") ||
           (t1 === !!!"uint32" && t2 === !!!"uint32") ||
           (t1 === !!!"double" && t2 === !!!"double") ||
           (t1 === !!!"float" && t2 === !!!"float") then
          t1
        else
          raise(TypeError(sprintf "Given %s and %s but expected numeric arguments" (t1.ToString()) (t2.ToString())))
    | Value arg -> 
        match arg with
        | Literal(l,_) -> getLiteralType l
        | Id(id,p) -> getLocalType id locals p
        | _ -> failwith "There is something wrong with the value of an arithmetic expression"
    | Nested expr -> checkArithmeticExpr expr position
            
  
  match premise with
  | Arithmetic(expr,result,position) ->
      let exprType = checkArithmeticExpr expr position
      { locals with Variables = locals.Variables |> Map.add result (exprType,position)},Arithmetic(expr,result,position) 
  | FunctionCall(func,result) ->
      let normFunc = normalizeDataOrFunctionCall symbolTable func locals
      let funcType,_ = checkNormalizedCall normFunc symbolTable locals false
      let locals,normalizedRes = checkFunctionCallResult result funcType
      locals,FunctionCall(normFunc,normalizedRes)
  | Bind(id,pos,expr) ->
      match expr with
      | NestedExpression(expr) ->
          let normData = normalizeDataOrFunctionCall symbolTable expr locals
          let dataType,_ = checkNormalizedCall normData symbolTable locals false
          { locals with Variables = locals.Variables |> Map.add id (dataType,pos) },Bind(id,pos,NestedExpression(normData))
      | Literal(l,p) ->
          let litType = getLiteralType l
          { locals with Variables = locals.Variables |> Map.add id (litType,pos) },premise
      | Id(rightId,pos) ->
          let idOpt = locals.Variables.TryFind(rightId)
          match idOpt with
          | Some(idType,_) -> 
              { locals with Variables = locals.Variables |> Map.add id (idType,pos) },premise
          | None ->
              undefinedVarError id.Name pos
      | Lambda _ -> failwith "Anonymous functions not supported yet"
  | Emit(_,ret,pos) ->
      { locals with Variables = locals.Variables |> Map.add ret (Unsafe,pos)},premise    
  | Conditional(left,op,right) ->
      match op with
      | Equal
      | NotEqual ->
          match left,right with
          | Id(id,p),Literal(l,_)
          | Literal(l,_),Id(id,p) ->
              let litType = getLiteralType l
              let idType,_ = checkSingleArg (Id(id,p)) symbolTable litType locals false
              locals,premise
          | Literal(l1,p1),Literal(l2,p2) ->
              let t1 = getLiteralType l1
              let litType,_ = checkLiteral l2 t1 p2 symbolTable locals
              locals,premise
          | NestedExpression(expr),Id(id,p)
          | Id(id,p),NestedExpression(expr) ->
              let idType = getLocalType id locals p
              let dataType = checkSingleArg (Id(id,p)) symbolTable idType locals false
              locals,premise
          | NestedExpression(expr1),NestedExpression(expr2) ->
              //use checkNormalizedCall to extract one of the types and then compare it with the other
              let normData = normalizeDataOrFunctionCall symbolTable expr1 locals
              let dataType1,_ = checkNormalizedCall normData symbolTable locals false
              let dataType2,_ = checkSingleArg right symbolTable dataType1 locals false
              locals,premise
          | Id(id1,p1),Id(id2,p2) ->
              let idType1 = getLocalType id1 locals p1
              let idType2 = checkSingleArg (Id(id2,p2)) symbolTable idType1 locals false
              locals,premise
          | NestedExpression(expr),Literal(l,_)
          | Literal(l,_), NestedExpression expr ->
              let literalType = getLiteralType l
              let dataType,_ = checkSingleArg (NestedExpression(expr)) symbolTable literalType locals false
              locals,premise
          // Add the case of the comparison of two nested expressions
          | _ -> failwith "Equality case not implemented yet"
      | Less
      | LessEqual
      | Greater
      | GreaterEqual ->
          match left,right with
          | Id(id,pos),Literal(l,litPos)
          | Literal(l,litPos),Id(id,pos) ->
              let literalType = getLiteralType l   
              match l with
              | I64 _
              | U64 _
              | I32 _
              | U32 _
              | F64 _
              | F32 _ ->              
                  let idType,_ = checkSingleArg (Id(id,pos)) symbolTable literalType locals false
                  locals,premise
              | _ ->
                  raise(TypeError(sprintf "The type %s at %s is not valid for this comparison" (literalType.ToString()) (litPos.ToString())))
          | Literal(l1,p1),Literal(l2,p2) ->
              let type1 = getLiteralType l1
              let type2 = getLiteralType l2
              do checkTypeEquivalence type1 type2 p1 symbolTable locals
              locals,premise
          | _ -> raise(TypeError("With this operator you cannot compare data structures"))
      | _ -> failwith "Predicate not implemented yet..."



and checkRule (rule : RuleDefinition) (symbolTable : SymbolContext) =
  match rule with
  | Rule(r) ->
    match r.Conclusion with
    | ValueOutput(call,result) ->
        let normalizedCall = normalizeDataOrFunctionCall symbolTable call LocalContext.Empty
        let callType,locals = checkNormalizedCall normalizedCall symbolTable LocalContext.Empty true
        let localsAfterPremises,normPremises =
          r.Premises |> List.fold(fun (l,pr) p -> 
                                  let loc,prem = checkPremise p symbolTable l
                                  loc,prem :: pr) (locals,[])
        let normPremises = normPremises |> List.rev
        match result with
        | [NestedExpression(expr)] ->
            let normalizedRes = normalizeDataOrFunctionCall symbolTable expr LocalContext.Empty
            do checkNormalizedCall normalizedRes symbolTable localsAfterPremises false |> ignore
            (Rule( { Main = r.Main; Premises = normPremises; Conclusion = ValueOutput(normalizedCall,normalizedRes)})),(callType,localsAfterPremises) 
        | [arg] ->
            do checkSingleArg arg symbolTable callType localsAfterPremises false |> ignore
            (Rule( { Main = r.Main; Premises = normPremises; Conclusion = ValueOutput(normalizedCall,result)})),(callType,localsAfterPremises)
        | x :: xs ->
            let normalizedRes = normalizeDataOrFunctionCall symbolTable result LocalContext.Empty
            do checkNormalizedCall normalizedRes symbolTable localsAfterPremises false |> ignore
            (Rule( { Main = r.Main; Premises = normPremises; Conclusion = ValueOutput(normalizedCall,normalizedRes)})),(callType,localsAfterPremises) 
        | _ -> failwith "Why is the result of a conclusion empty?"          
    | ModuleOutput(_) ->
        raise(TypeError("You can only output modules in a type rule"))
  | TypeRule(tr) -> failwith "type rules not supported yet..."


and buildSubTypes (subTypesDef : List<TypeDecl * TypeDecl>) : Map<TypeDecl,List<TypeDecl>> =
  subTypesDef |> List.fold(fun sts (t,alias) ->
                              let subTypeOpt = sts |> Map.tryFind t
                              match subTypeOpt with
                              | Some _ -> 
                                  sts |> Map.add t (alias :: (sts.[t]))
                              | None ->
                                  sts |> Map.add t [alias]) Map.empty

and checkProgramDefinition (_module : string) (imports : List<string>) (programDefinition : ProgramDefinition) : TypedProgramDefinition = 
  let symbolTable = buildSymbols programDefinition.Declarations Map.empty
  do checkSymbols programDefinition.Declarations symbolTable
  let symbolTable = { symbolTable with Subtyping = buildSubTypes programDefinition.Subtyping }
  if programDefinition.Rules |> 
      List.filter(fun x ->
                    match x with
                    | Rule r -> r.Main
                    | TypeRule _ -> false) |> List.length > 1 then
    raise(TypeError("A program cannot contain more than one entry point"))
  else
    let typedRules =
      [for r in programDefinition.Rules do
          match r with
          | Rule(r1) ->
              let normRule,(_type,locals) = checkRule r symbolTable
              match normRule with
              | Rule(nr1) ->
                  let typedRule = { Main = nr1.Main; Premises = nr1.Premises; Conclusion = nr1.Conclusion; Locals = locals; ReturnType = _type }
                  yield TypedRule(typedRule)
              | TypeRule(nr) -> failwith "Type rule not supported yet..."
          | TypeRule(r) -> failwith "Type rule not supported yet..."]
    {
      Module = _module
      Declarations = programDefinition.Declarations
      TypedRules = typedRules
      SymbolTable = symbolTable
    }

and checkProgram (program : Program) : TypedProgramDefinition =
  //missing support for imports
  let _namespace,imports,def = program.Namespace,program.Imports,program.Program
  checkProgramDefinition _namespace imports def


