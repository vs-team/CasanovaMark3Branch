module CodeGenerationCsharp

open ParserAST
open TypeChecker
open Common
open DefaultMappings

exception CodeGenerationError of string

let resultStruct = "__MetaCnvResult"
let resultValue tabs typeSymbol valueSymbol = sprintf "%s__res = new %s<%s>();\n%s__res.Value = %s;\n%s__res.HasValue = true;" tabs resultStruct typeSymbol tabs valueSymbol tabs
let resultNone tabs typeSymbol =  sprintf "%s__res = new %s<%s>();\n%s__res.Value = default(%s);\n%s __res.HasValue = false;" tabs resultStruct typeSymbol tabs typeSymbol tabs

type CodeGenerationCtxt =
  {
    Program                   : TypedProgramDefinition
    Code                      : string
    CurrentTabs               : int
    ArgIndex                  : int
    RuleIndex                 : int
    TempIndex                 : int
    CurrentDataVar            : string
    CurrentResTmp             : string
    CurrentRuleTmp            : string
    PremiseResultTemps        : string list
    GeneratedTemps            : string list
    GeneratedInterfaces       : string list
    CurrentRuleRetType        : TypeDecl
  }
  static member Init(program : TypedProgramDefinition) = 
    {
      Program = program; Code = ""
      CurrentTabs = 0
      ArgIndex = 0
      RuleIndex = 0
      TempIndex = 0
      CurrentDataVar = ""
      CurrentResTmp = ""
      CurrentRuleTmp = ""
      PremiseResultTemps = []
      GeneratedTemps = []
      GeneratedInterfaces = []
      CurrentRuleRetType = Zero
    }
  member this.TempName index = "__tmp" + (string index)
  member this.LastTempCode = if (this.TempIndex -  1) < 0 then "" else (this.TempName (this.TempIndex - 1))
  member this.CurrentTempCode = this.TempName this.TempIndex
  member this.AddTemp =
    let newIndex = this.TempIndex + 1 
    { this with 
        TempIndex = newIndex
        GeneratedTemps = this.CurrentTempCode :: this.GeneratedTemps |> List.rev 
    }
//  member this.TempDottedPath = if this.GeneratedTemps.Length = 0 then "" else this.LastTempCode//this.GeneratedTemps |> List.reduce(fun temp1 temp2 -> temp1 + "." + temp2)
  member this.ResetArgs = { this with ArgIndex = 0 }


let emitResultTempCondition (tmps : string list) =
  match tmps with
  | [] -> ""
  | [tmp] -> sprintf "%s.HasValue" tmp
  | _ -> tmps |> List.reduce (fun x y -> sprintf "%s.HasValue || %s.HasValue" x y)

let symbolUsedInSubtypes (decl : SymbolDeclaration) (subtypes : Map<TypeDecl,List<TypeDecl>>) =
  subtypes |> Map.exists(fun _ ts -> ts |> List.exists(fun t -> t === decl.Return))

let getTypeSimpleName (t : TypeDecl) =
  match t with
  | Arg(Id(id,_),_) -> id.Name
  | _ -> failwith "The return argument of a Data structure should be an identifier..."

//let getTypeInterface (t : TypeDecl) = "I" + (getTypeSimpleName t)

let getReturnTypeSimpleName (decl : SymbolDeclaration) = getTypeSimpleName decl.Return

let getDeclInterface (decl : SymbolDeclaration) =
  getReturnTypeSimpleName decl

let rec emitTabs tabs =
   if tabs = 0 then
    "\t"
   else
    "\t" + (emitTabs (tabs - 1))

let rec emitType (t : TypeDecl) =
  match t with
  | Arg(Id(id,_),_) ->
      let defaultMapping = typeMappingsCsharp |> Map.tryFind (Arg(Id(id,emptyPos),[])) //[] is a placeholder until generics work
      match defaultMapping with
      | Some mapping -> mapping
      | None ->
          id.ToString()
  | Arrow(t1,t2,_) ->
      "Func<" + (emitType t1) + "," + (emitType t2) + ">"
  | _ -> failwith "unsupported TypeDecl format in emitType"

let emitId (id : Id) (useNameSpace : bool) = 
  if useNameSpace then id.ToString() else id.Name

let emitReturnArg (ctxt : CodeGenerationCtxt) (retType : TypeDecl) =
  (emitTabs ctxt.CurrentTabs) + "public " +  resultStruct + "<" + 
  (emitType retType) +
  ">" + " __res" + ";\n"

let emitLocals (ctxt : CodeGenerationCtxt) (vars : Map<Id,TypeDecl * Position>) =
  Map.fold(fun code id (t,_) ->
                code + (emitTabs ctxt.CurrentTabs) + "public " + (emitType t) + " " + (emitId id false) + ";\n") "" vars

let rec findArgType (index : int) (t : TypeDecl) =
  match t with
  | Arrow(t1,t2,_) ->
      if index = 0 then t1 else (findArgType (index - 1) t2)
  | _ ->
      if index > 0 then failwith "Something went wrong with the type argument lookup" else t

let emitNonVariableArgs (ctxt : CodeGenerationCtxt) (conclusion : Conclusion) =
  match conclusion with
  | ValueOutput(call,_) ->
    match call with
    | [fName] -> ""
    | fName :: args ->
      match fName with
      | Id(id,_) ->
        let argType = (ctxt.Program.SymbolTable.FuncTable |> Map.find id).Args
        List.fold(fun code arg ->
                let argIndex = args |> List.findIndex(fun x -> x = arg)
                let tabs = emitTabs ctxt.CurrentTabs
                let t = findArgType argIndex argType
                match arg with
                | Id _ -> code + ""
                | Literal _
                | NestedExpression _ ->
                    code + tabs + "public " + (emitType t) + " __arg" + (string argIndex) + ";\n"            
                | CallArg.Lambda _ -> failwith "To be implemented"
                    ) "" args
      | _ -> failwith "Function name is not an id???"
    | _ -> failwith "No arguments in the left part of the conclusion????"
  | ModuleOutput _ -> failwith "Modules not supported yet"

let composeArgPath (ctxt : CodeGenerationCtxt) (argIndex : int) (prefix : string) =
  if prefix = "" then
    (if ctxt.GeneratedTemps.Length = 0 then "" else ctxt.LastTempCode + ".") + "__arg" + (string argIndex)
  else
    prefix + ".__arg" + (string argIndex)


let emitReflectionStructuralCheck (ctxt : CodeGenerationCtxt) (dataSymbol : SymbolDeclaration) (index : int) (prefix : string) =
  let tabs = emitTabs ctxt.CurrentTabs
  let fullName = if prefix = "" then composeArgPath ctxt index ctxt.CurrentDataVar else prefix
  let opName = renameOperator dataSymbol.Name.Name
  let updatedCtxt = ctxt.AddTemp
  let code =
    (sprintf "\n%sif (!(%s is %s)) \n%s{\n%s\n%sreturn;\n%s}\n%s%s %s = (%s)%s;" 
      tabs 
      fullName
      (renameOperator dataSymbol.Name.Name )
      tabs
      (resultNone (emitTabs (ctxt.CurrentTabs + 1)) ((emitType ctxt.CurrentRuleRetType)))
      (emitTabs (ctxt.CurrentTabs + 1))
      tabs
      tabs
      opName
      ctxt.CurrentTempCode
      opName
      fullName)
  { updatedCtxt with Code = updatedCtxt.Code + code; CurrentDataVar = ctxt.CurrentTempCode }

let rec emitStructuralCheck (ctxt : CodeGenerationCtxt) (args : CallArg list) (prefix : string) =
  args |>
  List.fold(fun (index,newCtxt) arg ->
              let tabs = emitTabs newCtxt.CurrentTabs
              let composedArg = composeArgPath newCtxt index newCtxt.CurrentDataVar
              match arg with
              | Literal(l,_) ->
                  let code =
                     (sprintf "\n%sif (%s != %s)\n%s{\n%s\n%sreturn;\n%s}" 
                     tabs 
                     composedArg
                     (l.ToString()) 
                     tabs 
                     (resultNone (emitTabs (newCtxt.CurrentTabs + 1)) (emitType newCtxt.CurrentRuleRetType))
                     (emitTabs (newCtxt.CurrentTabs + 1)) tabs)
                  index + 1,{ newCtxt with Code = newCtxt.Code + code }
              | NestedExpression nestedArgs ->
                  let dataName = nestedArgs.Head
                  let arguments = nestedArgs.Tail
                  match dataName with
                  | Id(id,_) ->
                    let dataSymbol = newCtxt.Program.SymbolTable.DataTable.[id]
                    let updatedCtxt = emitReflectionStructuralCheck newCtxt dataSymbol index ""
                    let _,nestedCtxt = emitStructuralCheck updatedCtxt arguments prefix
                    index + 1,{ nestedCtxt with CurrentDataVar = newCtxt.CurrentDataVar }
                  | _ -> failwith "Data name is not an id???"
              //if you have a constructor with zero arguments it is an id, so remember to put here the code to check if the structure is correct if the id is a data constructor.
              | Id(id,_) ->
                  match newCtxt.Program.SymbolTable.DataTable |> Map.tryFind id with
                  | Some symbol when symbol.Args = Zero ->
                      let updatedCtxt = emitReflectionStructuralCheck newCtxt symbol index prefix
                      (index + 1),updatedCtxt
                  | _ -> (index + 1),newCtxt
              | CallArg.Lambda _ -> (index + 1),newCtxt) (0,ctxt)

let emitConclusionCheck (ctxt : CodeGenerationCtxt) (rule : TypedRule) =
  let tabs = emitTabs ctxt.CurrentTabs
  let innerTabs = emitTabs (ctxt.CurrentTabs + 1)
  let resultCreation =
    sprintf "%s__res = new %s<%s>();\n%s__res.Value = default(%s);\n%s__res.HasValue = false;\n"
      innerTabs
      resultStruct 
      (emitType ctxt.CurrentRuleRetType)
      innerTabs
      (emitType ctxt.CurrentRuleRetType)
      innerTabs
  match rule.Conclusion with
  | ValueOutput(call,res) ->
      match call with
      | [arg] ->
          { ctxt
              with
                //leave the bracket open because it will be completed in a following generation function
                Code = ctxt.Code +  tabs + "public void Run()\n" + tabs + "{" + "\n" + tabs + "\n" + resultCreation 
          }
      | fName :: args ->
          let _,updatedCtxt = emitStructuralCheck {ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } args ""
          { ctxt
              with
                 //leave the bracket open because it will be completed in a following generation function
                Code = ctxt.Code + tabs + "public void Run()\n" + tabs + "{" + updatedCtxt.Code + "\n" + tabs + "\n" + resultCreation
                TempIndex = updatedCtxt.TempIndex
                GeneratedTemps = updatedCtxt.GeneratedTemps
          }
      | _ -> failwith "Call is empty?!!"
  | ModuleOutput _ -> failwith "Module generation not supported yet"

let rec emitDataVariableCopy (ctxt : CodeGenerationCtxt) (args : CallArg list) =
  match args with
  | dataName :: args ->
      args |>
      List.fold(fun (index,newCtxt) arg ->
                  match arg with
                  | Literal(l,_) -> index + 1,newCtxt
                  | NestedExpression expr ->
                      emitDataVariableCopy newCtxt expr
                  | Id(id,_) ->
                      let tabs = emitTabs (newCtxt.CurrentTabs + 1)
                      let copyCode =
                        sprintf "%s%s = %s.__arg%d;\n"
                          tabs
                          id.Name
                          newCtxt.CurrentDataVar
                          index
                      index + 1,{ newCtxt with Code = newCtxt.Code + copyCode }
                  | CallArg.Lambda _ -> failwith "Lambdas not supported yet") (0,ctxt)
  | _ -> failwith "Something is quite wrong, my dear..."

let emitResultReflectionStructuralCheck (ctxt : CodeGenerationCtxt) (dataSymbol : SymbolDeclaration) (index : int) (prefix : string) =
  let tabs = emitTabs ctxt.CurrentTabs
  let fullName = if prefix = "" then composeArgPath ctxt index ctxt.CurrentDataVar else prefix
  let opName = renameOperator dataSymbol.Name.Name
  let updatedCtxt = ctxt.AddTemp
  let code =
    (sprintf "\n%sif (!(%s is %s)) \n%s{\n%s%s.HasValue = false;\n%s}\n%selse\n%s{\n%s%s %s = (%s)%s;\n" 
      tabs 
      fullName
      (renameOperator dataSymbol.Name.Name )
      tabs
      (emitTabs (updatedCtxt.CurrentTabs + 1))
      updatedCtxt.CurrentResTmp
      tabs
      tabs
      tabs
      (emitTabs (updatedCtxt.CurrentTabs + 1))
      opName
      updatedCtxt.CurrentTempCode
      opName
      fullName)
  { updatedCtxt with Code = updatedCtxt.Code + code; CurrentDataVar = updatedCtxt.CurrentTempCode }



let rec emitResultStructuralCheck (ctxt : CodeGenerationCtxt) (resultArgs : CallArg list) (prefix : string) =
  resultArgs |>
  List.fold (fun (index,newCtxt) arg ->
                let tabs = emitTabs newCtxt.CurrentTabs
                let innerTabs = emitTabs (newCtxt.CurrentTabs + 1)
                let composedArg = composeArgPath newCtxt index ctxt.CurrentDataVar
                match arg with
                | Literal (l,_) ->
                    let code =
                      (sprintf "\n%sif (%s != %s)\n%s{\n%s%s.HasValue = false;\n%s}\n"
                          tabs
                          composedArg
                          (l.ToString())
                          tabs
                          innerTabs
                          newCtxt.CurrentResTmp
                          tabs)
                    (index + 1,{ newCtxt with Code = newCtxt.Code + code })
                | NestedExpression nestedArgs ->
                    let dataName = nestedArgs.Head
                    let arguments = nestedArgs.Tail
                    match dataName with
                    | Id(id,_) ->
                        let dataSymbol = newCtxt.Program.SymbolTable.DataTable.[id]
                        let updatedCtxt = emitResultReflectionStructuralCheck newCtxt dataSymbol index ""
                        index + 1,
                        { updatedCtxt with CurrentDataVar = newCtxt.CurrentDataVar; Code = updatedCtxt.Code; CurrentTabs =  newCtxt.CurrentTabs }
                    | _ -> failwith "Data name is not an id???"
                | Id(id,_) -> index + 1,newCtxt            
                | CallArg.Lambda _ -> failwith "Lambdas not supported yet") (0,ctxt)

let rec emitResultCopy (ctxt : CodeGenerationCtxt) (matchingRule : TypedRule) (args : CallArg list) =
      match args with
      | [Id(id,_)] when ctxt.Program.SymbolTable.DataTable.TryFind(id).IsNone ->
          let tabs = emitTabs ctxt.CurrentTabs
          let copyCode =
            sprintf "%s%s = %s.__res.Value;\n%s%s = %s.__res;\n"
              tabs
              id.Name
              ctxt.CurrentRuleTmp
              tabs
              ctxt.CurrentResTmp
              ctxt.CurrentRuleTmp
          { ctxt with Code = ctxt.Code + copyCode }
      | [NestedExpression(expr)] ->
          emitResultCopy ctxt matchingRule expr
      | arg :: args ->
          match arg with
          | Id(id,_) ->
              let dataOpt = ctxt.Program.SymbolTable.DataTable.TryFind(id)
              match dataOpt with
              | Some decl ->
                  let tabs = emitTabs ctxt.CurrentTabs
                  let copyCode =
                    sprintf "%s%s = %s.__res;\n"
                      tabs
                      ctxt.CurrentResTmp
                      ctxt.CurrentRuleTmp
                  let copyCtxt = { ctxt with Code = ctxt.Code + copyCode }
                  let operatorCheckCtxt = emitResultReflectionStructuralCheck copyCtxt decl 0 (copyCtxt.CurrentResTmp + ".Value")
                  let _,copyDataCtxt = emitDataVariableCopy operatorCheckCtxt (arg :: args)
                  let _,innerCtxt = emitResultStructuralCheck { copyDataCtxt with CurrentTabs = copyDataCtxt.CurrentTabs + 1 } args ""
                  { innerCtxt with CurrentDataVar = copyCtxt.CurrentDataVar; Code = innerCtxt.Code + (sprintf "%s}\n" tabs) }
              | None -> failwith "The data constructor does not exist??!! TypeChecker pls..."
          | _ -> failwith "Not an id ??!!"
      | _ -> failwith "The data constructor does not exist??!! TypeChecker pls..."

let emitExistingResultCheck (ctxt : CodeGenerationCtxt) (matchingRuleDef : TypedRule) (resultArgs : CallArg list)  =
  let tabs = emitTabs (ctxt.CurrentTabs + 1)
  let blockTabs = emitTabs (ctxt.CurrentTabs + 2)
  //let newCtxt = ctxt.AddTemp
  let existingCond = emitResultTempCondition ctxt.PremiseResultTemps.Tail
  let resultCheck =
    sprintf "%sif (%s%s.__res.HasValue)\n%s{\n" 
            tabs 
            (if existingCond = "" then "" else "!(" + existingCond + ") && ")
            (ctxt.TempName (ctxt.TempIndex - 2))
            tabs 
  let newCtxt = { ctxt with Code = ctxt.Code + resultCheck }
  let resultCopyCtxt = emitResultCopy { newCtxt with CurrentTabs = ctxt.CurrentTabs + 2} matchingRuleDef resultArgs
  { resultCopyCtxt with CurrentTabs = ctxt.CurrentTabs; Code = resultCopyCtxt.Code + (sprintf "\n%s}\n" tabs) }


let rec emitPremiseResultCheck (ctxt : CodeGenerationCtxt) (matchingRuleDef : TypedRuleDefinition) (resultArgs : CallArg list) =
  match matchingRuleDef with
  | TypedRule(matchingRule) ->
    let ctxtAfterResCheck = emitExistingResultCheck ctxt matchingRule resultArgs
    ctxtAfterResCheck
  | TypedTypeRule _ -> failwith "Type rule not supported yet"



let emitRuleCall (ctxt : CodeGenerationCtxt) (args : CallArg list) (ret : CallArg list) (rule : TypedRuleDefinition) (matchingRuleIndex : int) (firstPremise : bool) =
  let currentRuleDef = ctxt.Program.TypedRules.[ctxt.RuleIndex]
  match currentRuleDef,rule with
  | TypedRule(ctr),TypedRule(tr) ->
      match ctr.Conclusion,tr.Conclusion with
      | ValueOutput(_,currentRes),ValueOutput(call,res) ->
          let tabs = emitTabs (ctxt.CurrentTabs + 1)
          //let updatedCtxt = ctxt.AddTemp
          let ruleCreation (ctxt : CodeGenerationCtxt) =
            let ruleCode =
              sprintf "%sRule%d %s = new Rule%d();\n" 
                tabs 
                matchingRuleIndex 
                ctxt.CurrentTempCode 
                matchingRuleIndex
            { ctxt with CurrentRuleTmp = ctxt.CurrentTempCode; Code = ctxt.Code + ruleCode }
          let outputLiteralOrId (ctxt : CodeGenerationCtxt) (ruleArg : CallArg) (valueString : string) (isConstructor : bool) =
            match ruleArg with
            | Literal _ -> 
                let literalArg = sprintf "%s%s.__arg%d = %s;\n" tabs ctxt.CurrentTempCode ctxt.ArgIndex valueString
                let ruleCtxt = ruleCreation ctxt
                { ruleCtxt with 
                    Code = ruleCtxt.Code + literalArg
                    ArgIndex = ctxt.ArgIndex + 1 }.AddTemp
            | Id(id,_) ->
                let idArg =
                  if isConstructor then
                    sprintf "%s%s.__arg%d = %s;\n" tabs ctxt.CurrentTempCode ctxt.ArgIndex valueString
                  else 
                    sprintf "%s%s.%s = %s;\n" tabs ctxt.CurrentTempCode id.Name valueString
                let ruleCtxt = ruleCreation ctxt
                { ruleCtxt with 
                    Code = ruleCtxt.Code + idArg
                    ArgIndex = ruleCtxt.ArgIndex + 1 }.AddTemp
            | _ -> failwith "Matching rule argument is not a literal or a variable?!!!"
          let rec outputArgumentCopy (ctxt : CodeGenerationCtxt) (args : CallArg list) (call : CallArg list) (isConstructor : bool) =
            List.fold2(fun newCtxt callArg ruleArg ->
                          match callArg with
                          | Literal(l,_) ->
                              let literalArg = outputLiteralOrId newCtxt ruleArg (l.ToString()) isConstructor
                              literalArg
                          | Id(id,_) ->
                              let idArg = outputLiteralOrId newCtxt ruleArg id.Name isConstructor
                              idArg
                          | NestedExpression expr ->
                              let constructorSymbol = expr.Head
                              let newCtxt = newCtxt.AddTemp
                              match constructorSymbol with
                              | Id(symbol,_) ->
                                  let constructorCode = sprintf "%s%s %s = new %s();\n" tabs symbol.Name newCtxt.CurrentTempCode symbol.Name
                                  let innerCtxt = outputArgumentCopy newCtxt.ResetArgs.AddTemp expr expr true
                                  { innerCtxt with ArgIndex = newCtxt.ArgIndex }
                              | _ -> failwith "First argument of nested expression is not an id???"
                          | CallArg.Lambda _ -> failwith "Lambdas not supported yet...") ctxt args.Tail call.Tail
          let generateTmpResult (ctxt : CodeGenerationCtxt) =
            let newCtxt = ctxt.AddTemp
            let resCode =
              sprintf "%s%s<%s> %s;\n%s%s.HasValue = false;\n"
                tabs
                resultStruct
                (emitType tr.ReturnType)
                newCtxt.LastTempCode
                tabs
                newCtxt.LastTempCode
            { newCtxt with CurrentResTmp = newCtxt.LastTempCode; Code = newCtxt.Code + resCode; PremiseResultTemps = newCtxt.LastTempCode :: newCtxt.PremiseResultTemps }
          let ctxtAfterArgumentCopy = outputArgumentCopy ctxt args call false
//          let existingResCondition =
//            if firstPremise then
//              ""
//            else
//              sprintf "%s.HasValue" (ctxtAfterArgumentCopy.TempName (ctxtAfterArgumentCopy.TempIndex - 2))        
          let runCode =
            if firstPremise then
              sprintf "%s%s.Run();\n" tabs ctxtAfterArgumentCopy.CurrentRuleTmp;
            else
              sprintf "%sif(!(%s)) %s.Run();\n"
                tabs
                (emitResultTempCondition ctxtAfterArgumentCopy.PremiseResultTemps)
                ctxtAfterArgumentCopy.CurrentRuleTmp
          let ctxtAfterArgumentCopy = generateTmpResult ctxtAfterArgumentCopy
          let ctxtAfterArgumentCopy = { ctxtAfterArgumentCopy with  ArgIndex = 0; Code = ctxtAfterArgumentCopy.Code + runCode }
          let ctxtAfterPremiseResultCheck = emitPremiseResultCheck ctxtAfterArgumentCopy rule ret
          ctxtAfterPremiseResultCheck
      | ModuleOutput _,ModuleOutput _ -> failwith "A normal rule outputs a module???"
      | _ -> failwith "Something is VERY wrong!"
  | TypedTypeRule _, TypedTypeRule _ -> failwith "Type Rules not supported yet"
  | _ -> failwith "Mixed rule definitions???? Something went very wrong!"

let emitRulesCall (ctxt : CodeGenerationCtxt) (args : CallArg list) (ret : CallArg list) (rules : (TypedRuleDefinition * int) list) = 
  rules |> 
  List.fold (fun newCtxt (rule,ruleIndex) -> emitRuleCall newCtxt args ret rule ruleIndex (rule = (fst rules.Head))) ctxt
  
// When implementing lambdas, here you should check if the argument is among the local variables and if it is a lambda. If that is the case then just call the lambda.
let emitFunctionCall (ctxt : CodeGenerationCtxt) (functionCall : CallArg list * CallArg list) =
  let call,ret = functionCall
  let matchingRules = ctxt.Program.TypedRules |>
                      List.mapi (fun i x -> (x,i)) |> 
                      List.filter(fun (ruleDef,ruleIndex) ->
                                    match ruleDef with
                                    | TypedRule(tr) -> 
                                        match tr.Conclusion with
                                        | ValueOutput(c,_) ->
                                            match call.Head,c.Head with
                                            | Id(id1,_),Id(id2,_) -> id1.Name = id2.Name
                                            | _ -> failwith "The first argument is not an id??!!"
                                        | ModuleOutput _ -> failwith "Module generation not supported yet"
                                    | TypedTypeRule(tr) -> failwith "Type Rules not supported yet")
  if matchingRules.Length > 0 then
    emitRulesCall { ctxt with PremiseResultTemps = [] } call ret matchingRules
  else
    raise(CodeGenerationError(sprintf "A premise in Rule %d will never be executed" ctxt.RuleIndex))

let emitPremises (ctxt : CodeGenerationCtxt) (premises : Premise list) =
  premises |>
  List.fold (fun newCtxt p ->
                match p with
                | FunctionCall(args,res) -> emitFunctionCall newCtxt (args,res)
                | Bind _
                | Conditional _ -> failwith "Conditionals not implemented yet...") ctxt

let emitResultDataStructure (ctxt : CodeGenerationCtxt) (symbol : SymbolDeclaration) =
  let tabs = emitTabs ctxt.CurrentTabs
  let updatedCtxt = ctxt.AddTemp
  let opName = renameOperator symbol.Name.Name
  let resultCode =
    sprintf "%s%s %s = new %s();\n"
      tabs
      opName
      updatedCtxt.CurrentTempCode
      opName
  { updatedCtxt with Code = updatedCtxt.Code + resultCode; CurrentResTmp = updatedCtxt.CurrentTempCode }

let rec emitResultDataArgGeneration (ctxt : CodeGenerationCtxt) (args : CallArg list) = 
  args |>
  List.fold(fun (index,newCtxt) arg ->
              let tabs = emitTabs ctxt.CurrentTabs
              match arg with
              | Literal(l,_) ->
                  let copyCode =
                    sprintf "%s%s.__arg%d = %s;\n"
                      tabs
                      newCtxt.CurrentResTmp
                      index
                      (l.ToString())
                  index + 1,{ newCtxt with Code = newCtxt.Code + copyCode }
              | Id(id,_) ->
                  let copyCode =
                    sprintf "%s%s.__arg%d = %s;\n"
                      tabs
                      newCtxt.CurrentResTmp
                      index
                      id.Name
                  index + 1,{ newCtxt with Code = newCtxt.Code + copyCode }
              | NestedExpression expr ->
                  let dataName = expr.Head
                  let nestedArgs = expr.Tail
                  match dataName with
                  | Id(id,_) ->
                      let dataSymbol = newCtxt.Program.SymbolTable.DataTable.[id]
                      let updatedCtxt = emitResultDataStructure newCtxt dataSymbol
                      let _,nestedCtxt = emitResultDataArgGeneration updatedCtxt nestedArgs
                      let copyCode =
                        sprintf "%s%s.__arg%d = %s;\n"
                          tabs
                          newCtxt.CurrentResTmp
                          index
                          nestedCtxt.CurrentResTmp
                      index + 1,{ nestedCtxt with Code = nestedCtxt.Code + copyCode; CurrentResTmp = newCtxt.CurrentResTmp }
                  | _ -> failwith "Data name is not an id???"
              | CallArg.Lambda _ -> failwith "Lambdas not supported yet") (0,ctxt)
    

let rec emitRuleResult (ctxt : CodeGenerationCtxt) (rule : TypedRule) =
  let result = rule.Conclusion
  match result with
  | ValueOutput (_,res) ->
      let tabs = emitTabs (ctxt.CurrentTabs + 1)
      match res with
      | [Literal(l,_)] ->
          let resultCode =
            sprintf "%s__res.HasValue = true;\n%s__res.Value = %s;\n"
              tabs
              tabs
              (l.ToString())
          { ctxt with Code = ctxt.Code + resultCode }
      | [Id(id,_)] when ctxt.Program.SymbolTable.DataTable.TryFind(id).IsNone ->
          let resultCode =
            sprintf "%s__res.HasValue = true;\n%s__res.Value = %s;\n"
              tabs
              tabs
              id.Name
          { ctxt with Code = ctxt.Code + resultCode }
      | [NestedExpression ((Id(id,_)) :: args)]
      | (Id(id,_)) :: args ->
          match ctxt.Program.SymbolTable.DataTable.TryFind(id) with
          | Some symbol ->
            let dataCtxt = emitResultDataStructure { ctxt with CurrentTabs = ctxt.CurrentTabs + 1} symbol
            let _,resCtxt = emitResultDataArgGeneration dataCtxt args
            let resultCode =
              sprintf "%s__res.HasValue = true;\n%s__res.Value = %s;\n"
                tabs
                tabs
                resCtxt.CurrentResTmp
            { resCtxt with Code = resCtxt.Code + resultCode }
          | None -> failwith "The data structure does not exist???"
      | _ -> failwith "Invalid structure of rule result"
  | ModuleOutput _ -> failwith "Something is wrong: a rule is returning a module"

let emitRule (ctxt : CodeGenerationCtxt) (rule : TypedRule) =
  let tabs = emitTabs ctxt.CurrentTabs
  let locals = emitLocals { ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } rule.Locals.Variables
  let nonVarArgs =
    match rule.Conclusion with
    | ValueOutput(call,res) ->
        emitNonVariableArgs { ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } rule.Conclusion
    | ModuleOutput _ -> failwith "Modules not supported yet"
  let returnArg =  emitReturnArg { ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } rule.ReturnType
  let conclusionCtxt = emitConclusionCheck { ctxt with CurrentTabs = ctxt.CurrentTabs + 1; Code = "" } rule
  let premiseCtxt = emitPremises conclusionCtxt rule.Premises
  let resultCtxt = emitRuleResult premiseCtxt rule
  let runFunction = resultCtxt.Code + (emitTabs (premiseCtxt.CurrentTabs)) + "}\n" 
  sprintf "%spublic class %s\n%s{\n%s%s%s%s%s}\n" tabs ("Rule" + (string ctxt.RuleIndex)) tabs locals nonVarArgs returnArg runFunction tabs

let emitRules (ctxt : CodeGenerationCtxt) : CodeGenerationCtxt =
  List.fold (fun newCtxt r -> 
                match r with
                | TypedRule tr ->
                    { newCtxt with 
                        Code = newCtxt.Code + (emitRule { newCtxt with CurrentRuleRetType = tr.ReturnType } tr)
                        RuleIndex = newCtxt.RuleIndex + 1
                        ArgIndex = 0
                        TempIndex = 0
                        CurrentRuleRetType = tr.ReturnType
                    }
                | TypedTypeRule _ -> newCtxt) ctxt ctxt.Program.TypedRules

let defaultHeader =
  "using System;\n"

let defaultClasses =
  "//-- BEGIN COMPILER-GENERATED CODE --\n\n
   public struct " +  resultStruct  +  "<T> { public T Value; public bool HasValue;  }\n
   //-- END OF COMPILER-GENERATED CODE --\n\n"

let rec emitDataArgs (ctxt : CodeGenerationCtxt) (t : TypeDecl) (currentIndex : int) : string =
  let tabs = emitTabs ctxt.CurrentTabs
  match t with
  | Arrow(t1,t2,false) ->
      tabs + "public " + (emitType t1) + " __arg" + (string currentIndex) + ";\n" + (emitDataArgs ctxt t2 (currentIndex + 1))
  | Arrow(t1,t2,true) ->
      match t2 with
      | Arg(_) -> tabs + "public " + (emitType t) + " __arg" + (string currentIndex) + ";\n"
      | Arrow _ -> tabs + "public " + (emitType t1) + " __arg" + (string currentIndex) + ";\n" + (emitDataArgs ctxt t2 (currentIndex + 1))
      | _ ->  failwith "Invalid type format in data declaration..."
  | Arg(Id(_),_) -> tabs + "public " + (emitType t) + " __arg" + (string currentIndex) + ";\n"
  | Zero -> ""
  | _ -> failwith "Invalid type format in data declaration..."

let emitSubTypes (ctxt : CodeGenerationCtxt) (decl : SymbolDeclaration) (placeComma : bool) =
  let subtypes = ctxt.Program.SymbolTable.Subtyping
  match subtypes |> Map.tryFindKey (fun t _ -> t === decl.Return) with
     | Some _ ->
        let types = subtypes.[subtypes |> Map.findKey (fun t _ -> t === decl.Return)]
        if types.Length = 0 then "" 
        else 
          (if placeComma then "," else "") +
          (types |> 
           List.map(fun t -> getTypeSimpleName t) |>
           List.reduce(fun t1 t2 ->
                              t1 + "," + t2))
     | None -> ""
  

let emitDataStructure (ctxt : CodeGenerationCtxt) (decl : SymbolDeclaration) : string =
  let tabs = emitTabs ctxt.CurrentTabs
  let subtypes = ctxt.Program.SymbolTable.Subtyping
  let interfaces =
    getDeclInterface decl +
    (emitSubTypes ctxt decl true)
  "public class " + (renameOperator decl.Name.Name) + (if interfaces <> "" then " : " + interfaces else "") + "\n" + tabs + "{\n" + (emitDataArgs {ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } decl.Args 0) + tabs + "}\n"

let emitDataStructures (ctxt : CodeGenerationCtxt) : CodeGenerationCtxt =
  let dataTable = ctxt.Program.SymbolTable.DataTable
  Map.fold(fun newCtxt data decl ->
              let retName = getReturnTypeSimpleName decl
              let tabs = emitTabs ctxt.CurrentTabs
              match newCtxt.GeneratedInterfaces |> List.tryFind(fun x -> retName = x) with
              | Some _ ->
                  { newCtxt with Code = newCtxt.Code + tabs + (emitDataStructure newCtxt decl) }
              | None ->
                let subtypes = ctxt.Program.SymbolTable.Subtyping
                let inheritanceCode = emitSubTypes ctxt decl false
                { newCtxt with 
                    Code = newCtxt.Code + tabs + "public interface " + 
                           retName + (if inheritanceCode <> "" then " : " + inheritanceCode else "") + "{ }\n" + tabs + emitDataStructure newCtxt decl
                    GeneratedInterfaces = retName :: newCtxt.GeneratedInterfaces }) ctxt dataTable

let emitMain (ctxt : CodeGenerationCtxt) =
  let entryPoint =
    ctxt.Program.TypedRules |>
    List.tryFindIndex
                  (fun x ->
                    match x with
                    | TypedRule(r) -> r.Main
                    | _ -> false)
  match entryPoint with
  | Some idx ->
      let tabs = emitTabs ctxt.CurrentTabs
      let innerTabs = emitTabs (ctxt.CurrentTabs + 1)
      let mainTabs = emitTabs (ctxt.CurrentTabs + 2)
      let mainCode =
        sprintf
          "%spublic class EntryPoint \n%s{\n%spublic static void Main(string[] args) \n%s{\n%s Rule%d __main = new Rule%d();\n%s__main.Run();\n%s}\n%s}"
          tabs
          tabs
          innerTabs
          innerTabs
          mainTabs
          idx
          idx
          mainTabs
          innerTabs
          tabs
      { ctxt with Code = ctxt.Code + mainCode }
  | None -> ctxt

let emitProgram (program : TypedProgramDefinition) =
  let startingCtxt = CodeGenerationCtxt.Init program
  sprintf
    "%s\nnamespace %s\n {\n%s%s%s%s\n}"
    defaultHeader (program.Module) defaultClasses (emitDataStructures startingCtxt).Code ((emitRules startingCtxt).Code) ((emitMain startingCtxt).Code)