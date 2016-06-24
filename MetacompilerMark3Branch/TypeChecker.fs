module TypeChecker

open Common
open ParserAST

exception TypeError of string

type LocalContext =
  {
    Variables : Map<Id,TypeDecl * Position>
    Generics  : Map<Id,TypeDecl>
  }
  with
    static member Empty =
      {
        Variables = Map.empty
        Generics = Map.empty
      }

type TypedProgramDefinition =
  {
    Declarations      : List<Declaration>
    TypedRules        : List<TypedRuleDefinition>
    SymbolTable       : SymbolContext
  }

and TypedRuleDefinition =
| TypedRule of TypedRule
| TypedTypeRule of TypedRule

and TypedRule = 
  {
    Premises        : List<Premise>
    Conclusion      : Conclusion
    Locals          : LocalContext
    ReturnType      : TypeDecl
  }


//built-in types. Maybe they are not necessary in the end because they are defined in the prelude. Leave them for debugging.
let builtInTypes =
  [
    "int64"
    "int"
    "unint64"
    "uint32"
    "string"
    "double"
    "float"
    "string"
    "bool"
    "void"
  ]

//extract function name from a CallArg and rearrange the term in the form: functioName arg1 arg2 ... argn. The same form data constructors
let rec normalizeDataOrFunctionCall (_symbolTable : SymbolContext) (args : List<ParserAST.CallArg>) : List<ParserAST.CallArg> =
  let normCall =
    args |> 
    List.fold(fun (fArg,args) arg ->
                match arg with
                | Literal _ ->
                    (fArg,arg :: args)
                | Id(s,_) ->
                    match fArg with
                    | [] ->
                        let funcOpt = _symbolTable.FuncTable |> Map.tryFindKey(fun name sym -> name.Name = s.Name)
                        let dataOpt = _symbolTable.DataTable |> Map.tryFindKey(fun name sym -> name.Name = s.Name)
                        match funcOpt with
                        | None ->
                            match dataOpt with
                            | Some _ ->
                                (arg :: fArg,args)
                            | None ->
                                (fArg,arg :: args)
                        | Some _ ->
                            (arg :: fArg,args)
                    | _ ->
                        (fArg,arg :: args)
                | Lambda(_) -> failwith "Anonymous functions not supported yet"
                | NestedExpression (nestedArgs) ->
                    (fArg,(NestedExpression(normalizeDataOrFunctionCall _symbolTable nestedArgs)) :: args)) ([],[])
  let argList = snd normCall |> List.rev
  let fArg = fst normCall
  match fArg with
  | [] -> raise(TypeError("Undefined function or data constructor"))
  | _ ->
    if fArg.Length > 1 then
      failwith "Something went wrong when normalizing data or function call: more than a function name found"
    else
      (fArg.Head) :: argList



let rec checkType (_type : TypeDecl) (symbolTable : SymbolContext) : TypeDecl =
  match _type with
  | Zero -> _type
  | Arrow(left,right) ->
      let leftType = checkType left symbolTable
      let rightType = checkType right symbolTable
      Arrow(leftType,rightType)
  | Arg(arg) ->
      match arg with
      | Id(arg,pos) ->
          let typeOpt = symbolTable.DataTable |> Map.tryFindKey(fun (k : Id) (s : SymbolDeclaration) -> 
                                                    match s.Return with
                                                    | Arg(sarg) ->
                                                        match sarg with
                                                        | Id(arg1,_) -> arg = arg1
                                                        | _ -> false
                                                    | _ -> false)
          match typeOpt with
          | Some id -> _type
          | None ->
              let builtInTypeOpt = builtInTypes |> List.tryFind(fun t -> arg.Name = t)
              match builtInTypeOpt with
              | Some _ -> _type
              | None -> raise(TypeError(sprintf "Type Error: Undefined type %s at %A" (_type.ToString()) (pos.Line,pos.Col)))
      | _ -> raise(TypeError(sprintf "Type Error: You cannot use Data constructors or literals in function declarations"))
  | Generic(id) -> Generic(id)

let buildSymbols (declarations : List<Declaration>) (symbols : Map<Id,SymbolDeclaration>) =
//  let check (symDecl : SymbolDeclaration) =
    
  declarations |> List.fold(fun sym decl ->
                              match decl with
                              | Data(data) ->
                                  //do checkType data.Args sym |> ignore
                                  match data.Return with
                                  | Arg(Id(arg,_)) ->
                                    {sym with DataTable = sym.DataTable.Add(data.Name,data)}
                                  | _ -> raise(TypeError(sprintf "Type Error: invalid type %s for the data %s" (data.Return.ToString()) data.Name.Name))
                              | Func(func) ->
                                  //do checkType func.Args sym |> ignore
                                  {sym with FuncTable = sym.FuncTable.Add(func.Name,func)}
                              | TypeFunc(tf) ->
                                  {sym with TypeFuncTable = sym.TypeFuncTable.Add(tf.Name,tf)}
                              | TypeAlias(ta) ->
                                  {sym with TypeAliasTable = sym.TypeAliasTable.Add(ta.Name,ta)}) SymbolContext.Empty

let checkSymbols (declarations : List<Declaration>) (symbolTable : SymbolContext) =
  for decl in declarations do
    match decl with
    | Data(data) ->
        do checkType data.Args symbolTable |> ignore
    | Func(func) ->
        do checkType func.Args symbolTable |> ignore
    | TypeFunc(tf) ->
        failwith "TypeFunctions not implemented yet..."
    | TypeAlias(ta) ->
        failwith "TypeAliases not implemented yet..."


let checkTypeWithErrorMsg t1 t2 p ctxt msg =
  if t1 = t2 || (TypeDecl.SubtypeOf t1 t2 ctxt.Subtyping) then
    ()
  else
    raise(TypeError(msg))

let checkGenericType (t1 : TypeDecl) (t2 : TypeDecl) (p : Position) (ctxt : SymbolContext) (locals : LocalContext) : LocalContext =
  match t2 with
  | Generic(id) ->
    let genericOpt = locals.Generics |> Map.tryFind id
    match genericOpt with
    | Some g ->
        do checkTypeWithErrorMsg t1 g p ctxt (sprintf "Generic variable %s has been bound to type %s but the given type is %s" (id.Name) (g.ToString()) (t1.ToString()))
        locals
    | None ->
        { locals with Generics = locals.Generics.Add(id,t1) }
  | _ -> failwith "checkGenericType can only check a generic argument"
      

let checkTypeEquivalence (t1: TypeDecl) (t2 : TypeDecl) (p : Position) (ctxt : SymbolContext)  =
  checkTypeWithErrorMsg t1 t2 p ctxt (sprintf "Type Error: given %s but expected %s at %s" (t1.ToString()) (t2.ToString()) (p.ToString()))

let checkTypeDecl t1 t2 p  ctxt locals =
  match t2 with
  | Generic(id) ->
      checkGenericType t1 t2 p ctxt locals
  | _ ->
      do checkTypeEquivalence t1 t2 p ctxt
      locals
    
let checkLiteral (l : Literal) (typeDecl : TypeDecl) (p : Position) (ctxt : SymbolContext) (locals : LocalContext) : TypeDecl * LocalContext =
    match l with
    | I64(_) ->
      !!"int64", checkTypeDecl !!"int64" typeDecl p ctxt locals
    | I32(_) ->
      !!"int", checkTypeDecl !!"int" typeDecl p ctxt locals
    | U64(_) ->
      !!"uint64", checkTypeDecl !!"uint64" typeDecl p ctxt locals
    | U32(_) ->
      !!"uint32", checkTypeDecl !!"uint32" typeDecl p ctxt locals
    | F64(_) ->
      !!"double", checkTypeDecl !!"double" typeDecl p ctxt locals
    | F32(_) ->
      !!"float", checkTypeDecl !!"float" typeDecl p ctxt locals
    | String(_) ->
      !!"string", checkTypeDecl !!"string" typeDecl p ctxt locals
    | Bool(_) ->
      !!"bool", checkTypeDecl !!"bool" typeDecl p ctxt locals
    | Void ->
      !!"void", checkTypeDecl !!"void" typeDecl p ctxt locals


let rec checkSingleArg
  (arg : ParserAST.CallArg)
  (symbolTable : SymbolContext)
  (typeDecl : TypeDecl)
  (ctxt : LocalContext)
  (buildLocals : bool) : TypeDecl * LocalContext =

  match arg with
  | Literal(l,p) ->
      match typeDecl with
      | Arg(Id(id,_)) ->        
          checkLiteral l typeDecl p symbolTable ctxt
      | Generic(_) ->
          checkLiteral l typeDecl p symbolTable ctxt
      | _ ->
          failwith "Something went wrong: the type definition has an invalid structure"
  | Id(id,p) ->
      if buildLocals then
        Arg(Id(id,p)),{ctxt with Variables = ctxt.Variables |> Map.add id (typeDecl,p)}
      else
        let idOpt = ctxt.Variables |> Map.tryFind id
        match idOpt with
        | Some (t,_) ->
            do checkTypeEquivalence t typeDecl p symbolTable
            t,ctxt             
        | None ->
            raise(TypeError(sprintf "Type Error: undefined variable %s at %s" id.Name (p.ToString())))
  | Lambda(_) -> failwith "Anonymous functions not supported yet"   
  | NestedExpression(call) ->
      let nestedType,nestedCtxt = checkNormalizedCall call symbolTable ctxt buildLocals
      match call.Head with
      | Id(id,p) ->
          let dataOpt = symbolTable.DataTable |> Map.tryFind(id)
          match dataOpt with
          | Some decl ->
              checkTypeEquivalence nestedType typeDecl p symbolTable
              nestedType,nestedCtxt
          | None -> 
              let funcOpt = symbolTable.FuncTable |> Map.tryFind(id)
              match funcOpt with
              | Some decl ->
                checkTypeEquivalence nestedType typeDecl p symbolTable
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
      | Arrow(left,right) ->
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
    else
      checkNormalizedArgs args symbolTable decl.FullType ctxt buildLocals

  match call with
  | arg :: args ->
      match arg with
      | Id(id,pos) ->
        let funcOpt = symbolTable.FuncTable |> Map.tryFind(id)
        let dataOpt = symbolTable.DataTable |> Map.tryFind(id)            
        match funcOpt with
        | None ->
            match dataOpt with
            | None ->
                failwith "You are checking arguments that are not data constructors or functions with checkNormalizedCall"
            | Some dSym ->
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
          newLocals  
      | id :: ids ->
          let normalizedData = normalizeDataOrFunctionCall symbolTable result
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
                do checkTypeEquivalence dataType funcType Position.Zero symbolTable
                newLocals
          | _ -> failwith "Something went wrong with the function normalizer: the first element is not an id"
      | _ -> failwith "Something went wrong: the return argument of a premise is empty"
  
  match premise with
  | FunctionCall(func,result) ->
      let normFunc = normalizeDataOrFunctionCall symbolTable func
      let funcType,_ = checkNormalizedCall normFunc symbolTable locals false
      checkFunctionCallResult result funcType
  | Conditional(conditional) -> failwith "Conditionals not implemented yet..."

and checkRule (rule : RuleDefinition) (symbolTable : SymbolContext) =
  match rule with
  | Rule(premises,conclusion) ->
    match conclusion with
    | ValueOutput(call,result) ->
        let normalizedCall = normalizeDataOrFunctionCall symbolTable call
        let callType,locals = checkNormalizedCall normalizedCall symbolTable LocalContext.Empty true
        let localsAfterPremises =
          premises |> List.fold(fun l p -> checkPremise p symbolTable l) locals
        match result with
        | [arg] ->
            checkSingleArg arg symbolTable callType localsAfterPremises false
        | x :: xs ->
            let normalizedRes = normalizeDataOrFunctionCall symbolTable result
            checkNormalizedCall normalizedRes symbolTable localsAfterPremises false 
        | _ -> failwith "Why is the result of a conclusion empty?"          
    | ModuleOutput(_) ->
        raise(TypeError("You can only output modules in a type rule"))
  | TypeRule(premises,conclusion) -> failwith "type rules not supported yet..."


and buildSubTypes (subTypesDef : List<TypeDecl * TypeDecl>) : Map<TypeDecl,List<TypeDecl>> =
  subTypesDef |> List.fold(fun sts (t,alias) ->
                              let subTypeOpt = sts |> Map.tryFind t
                              match subTypeOpt with
                              | Some _ -> 
                                  sts |> Map.add t (alias :: (sts.[t]))
                              | None ->
                                  sts |> Map.add t [alias]) Map.empty

and checkProgramDefinition ((decls,rules,subtypes) : ProgramDefinition) : TypedProgramDefinition = 
  let symbolTable = buildSymbols decls Map.empty
  do checkSymbols decls symbolTable
  let symbolTable = { symbolTable with Subtyping = buildSubTypes subtypes }
  let typedRules =
    [for r in rules do
        match r with
        | Rule(r1) ->
            let _type,locals = checkRule r symbolTable
            let typedRule = { Premises = fst r1; Conclusion = snd r1; Locals = locals; ReturnType = _type }
            yield TypedRule(typedRule)
        | TypeRule(r) -> failwith "Type rule not supported yet..."]
  {
    Declarations = decls
    TypedRules = typedRules
    SymbolTable = symbolTable
  }

and checkProgram ((moduleName,imports,def) : Program) : TypedProgramDefinition =
  //missing support for imports
  checkProgramDefinition def


