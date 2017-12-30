module CodeGenerationCSharp2

open ParserAST
open TypeChecker
open Common
open DefaultMappings
open CodeGenerationUtils

exception CodeGenerationError of string

let resultStruct = "__MetaCnvResult"
let resultValue tabs typeSymbol valueSymbol = sprintf "%s__res = new %s<%s>();\n%s__res.Value = %s;\n%s__res.HasValue = true;" tabs resultStruct typeSymbol tabs valueSymbol tabs
let resultNone tabs typeSymbol =  sprintf "%s__res = new %s<%s>();\n%s__res.Value = default(%s);\n%s __res.HasValue = false;" tabs resultStruct typeSymbol tabs typeSymbol tabs

type CodeGenerationCtxt =
  {
    Program                   : TypedProgramDefinition //used
    Code                      : string //used
    CurrentTabs               : int //used
    ArgIndex                  : int
    RuleIndex                 : int //used
    TempIndex                 : int //used
    GeneratedTemps            : string list //used
    GeneratedInterfaces       : string list //used
    CurrentRuleRetType        : TypeDecl //used
    CurrentFuncId             : Id //used
    RulesMatchingFunction     : int //used
    CurrentDataTemp           : int
  }
  static member Init(program : TypedProgramDefinition) = 
    {
      Program = program; Code = ""
      CurrentTabs = 0
      ArgIndex = 0
      RuleIndex = 0
      TempIndex = -1
      GeneratedTemps = []
      GeneratedInterfaces = []
      CurrentRuleRetType = Zero
      CurrentFuncId = { Name = ""; Namespace = "" } 
      RulesMatchingFunction = 0
      CurrentDataTemp = -1
    }
  member this.DataTemp = if this.CurrentDataTemp = -1 then "" else (this.TempName this.CurrentDataTemp) + "."
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
  member this.AddCode (code : string) = { this with Code = this.Code + code }

let rec emitTabs tabs =
   if tabs = 0 then
    ""
   else
    "\t" + (emitTabs (tabs - 1))


let emitArgCode argList tabs =
  argList |>
  List.fold(fun (i,code) typeName ->
              (i + 1,
                code + (sprintf "%spublic %s __arg%d;\n" tabs typeName i))) (0,"") |> snd

let emitGotoNextRule (ctxt : CodeGenerationCtxt) =
  if ctxt.RuleIndex = ctxt.RulesMatchingFunction - 1 then
    "goto default;"
  else
    sprintf "goto case %d;" (ctxt.RuleIndex + 1)


//[[Data "dataName" -> ... : Metatype1]]
//For each Data declaration define an empty interface for its meta-type
//If the meta-data is a subtype of another, than this interface also implements the interfaces for the supertypes.
let emitDataInterfaces (ctxt : CodeGenerationCtxt) =
  let tabs = emitTabs ctxt.CurrentTabs
  ctxt.Program.SymbolTable.DataTable |>
  Map.fold(fun newCtxt id decl ->
              let typeCode = getTypeSimpleName decl.Return
              //this avoids to generate the same interface twice.
              if newCtxt.GeneratedInterfaces |> List.contains(typeCode) then
                newCtxt
              else
                match ctxt.Program.SymbolTable.Subtyping |> Map.tryFindKey(fun k _ -> k === decl.Return) with
                | None ->                  
                      { newCtxt with 
                          Code = newCtxt.Code + (sprintf "%spublic interface %s{ }\n" tabs typeCode)
                          GeneratedInterfaces = typeCode :: newCtxt.GeneratedInterfaces }
                | Some _type ->
                    let subtypes = ctxt.Program.SymbolTable.Subtyping.[_type]
                    let subtypesString =
                      subtypes |>
                      List.map(fun decl -> getTypeSimpleName decl) |>
                      List.reduce(fun x y -> x + ", " + y)
                    let interfaceString =
                      sprintf "%spublic interface %s{ } : %s\n" tabs (getTypeSimpleName decl.Return) subtypesString
                    { newCtxt with 
                        Code = newCtxt.Code + interfaceString 
                        GeneratedInterfaces = typeCode :: newCtxt.GeneratedInterfaces}) ctxt

//[[Data "dataName" -> arg1 -> ... -> argn : Metatype1]]
//For each meta-data declaration define a class implementing the interface for the meta-type
//the class contains a field for each of the arguments needed to construct the meta-type.
//If the meta-data name contains symbols that are illegal in ids of C#, those are replaced with
//alternative names. The class contains a field __name storing the original symbols in the
//declaration for the pretty print.
let emitDataClasses (ctxt : CodeGenerationCtxt) =
  let tabs = emitTabs ctxt.CurrentTabs
  let classTabs = emitTabs (ctxt.CurrentTabs + 1)
  let methodTabs = emitTabs (ctxt.CurrentTabs + 2)
  ctxt.Program.SymbolTable.DataTable |>
  Map.fold(fun (newCtxt : CodeGenerationCtxt) id decl ->
            let argList =
              (extractTypeNamesFromTypeDecl decl.Args getTypeFullName)
            let argCode = emitArgCode argList classTabs
            let ToStringCode =
              let methodName = sprintf "%spublic override string ToString()\n" classTabs
              if argList.Length = 0 then
                sprintf "%s%s{\n%sreturn __name;\n%s}\n" methodName classTabs methodTabs classTabs
              else
                let argNames = 
                  argList |>
                  List.mapi(fun i _ -> sprintf "__arg%d" i) |>
                  List.fold(fun code i -> code + (sprintf " + \" \" + %s" i)) ""
                sprintf "%s%s{\n%sreturn \"(\" + __name %s + \")\";\n%s}\n" methodName classTabs methodTabs argNames classTabs
            let classCode =
              sprintf "%spublic class %s : %s \n%s{\n%spublic string name = \"%s\";\n%s%s%s}\n" 
                tabs 
                (renameOperator decl.Name.Name)
                (getTypeSimpleName decl.Return)
                tabs 
                classTabs
                decl.Name.Name
                argCode
                ToStringCode
                tabs
            newCtxt.AddCode classCode) ctxt

//Variables automatically pass the
//pattern matching. Literals must be checked against their value. Explicit data structures (like (a + b))
//must be recursively checked by testing the patterns of its arguments (which can be also explicit data structures).
//If the whole premise contains an explicit data structure this should be inserted into a NestedExpression before using
//this function.
let rec emitStructuralCheck (ctxt : CodeGenerationCtxt) (pattern : List<CallArg>) =                                  
  let tabs = emitTabs ctxt.CurrentTabs
  let ifBodyTabs = emitTabs (ctxt.CurrentTabs + 1)
  pattern.Tail |>
  List.fold(fun (i,newCtxt) arg ->
              match arg with
              | Literal(l,_) ->
                  let checkCode =
                      sprintf "%sif(__arg%d != %s)\n%s{\n%s%s;\n%s}"
                        tabs
                        i
                        (string l)
                        tabs
                        ifBodyTabs
                        (emitGotoNextRule newCtxt)
                        tabs
                  i + 1, { newCtxt with Code = newCtxt.Code + checkCode }
              | Id (id,_) ->
                  //if the premise left side contains only one argument it might be a function or a
                  //data structure taking no arguments
                  match newCtxt.Program.SymbolTable.FuncTable.TryFind(id) with
                  | Some _ ->
                      i + 1,newCtxt
                  | None ->
                      match newCtxt.Program.SymbolTable.DataTable.TryFind(id) with
                      | Some _ ->
                          i + 1,emitStructuralCheck newCtxt [NestedExpression([arg])]
                      | None ->
                          let varCopyCode =
                            sprintf "%s%s = %s__arg%d;\n"
                              tabs
                              id.Name
                              newCtxt.DataTemp
                              i
                          i + 1,{ newCtxt with Code = newCtxt.Code + varCopyCode }
              | NestedExpression(dataName :: args) ->
                  let (Id(id,_)) = dataName
                  let checkCode =
                    sprintf "%sif (!(%s__arg%d is %s.%s))\n%s{\n%s%s\n%s}\n"
                      tabs
                      newCtxt.DataTemp
                      i
                      id.Namespace
                      (renameOperator id.Name)
                      tabs
                      ifBodyTabs
                      (emitGotoNextRule newCtxt)
                      tabs
                  //data structures must be cast to a temporary variable to get their concrete type
                  let ctxtWithDataTemp = newCtxt.AddTemp
                  let ctxtWithDataTemp = { ctxtWithDataTemp with CurrentDataTemp = newCtxt.TempIndex }
                  let castCode =
                    sprintf "%s%s.%s %s = (%s.%s)%s__arg%d"
                      tabs
                      id.Namespace
                      (renameOperator id.Name)
                      ctxtWithDataTemp.LastTempCode
                      id.Namespace
                      (renameOperator id.Name)
                      newCtxt.DataTemp
                      i
                  let argumentStructuralCtxt = emitStructuralCheck { newCtxt with CurrentDataTemp = ctxtWithDataTemp.TempIndex} (dataName :: args)
                  i + 1,{ argumentStructuralCtxt with Code = newCtxt.Code + checkCode + castCode + argumentStructuralCtxt.Code }) (0,ctxt) |> snd


//Each case of a rule declares a set of local variables corresponding to the local variables of the rule
let emitRuleCase (ctxt : CodeGenerationCtxt) =
  let tabs = emitTabs ctxt.CurrentTabs
  let caseTabs = emitTabs (ctxt.CurrentTabs + 1)
  let caseBodyTabs = emitTabs (ctxt.CurrentTabs + 2)
  let (TypedRule(tr)) = ctxt.Program.TypedRules.[ctxt.RuleIndex]
  let (ValueOutput(left,right)) = tr.Conclusion
  let localVarsCode =
    tr.Locals.Variables |>
    Map.fold (fun code id (decl,_) ->
                sprintf "%s%s %s = default(%s);\n"
                  caseBodyTabs
                  (getTypeFullName decl)
                  id.Name
                  (getTypeFullName decl)) ""
  let structuralCheckCtxt =
    emitStructuralCheck { ctxt with CurrentTabs = ctxt.CurrentTabs + 2; Code = "" } left
  let caseCode =
    sprintf "%scase %d:\n%s{\n%s%s%sbreak;\n%s}\n"
      tabs
      ctxt.RuleIndex
      caseTabs
      localVarsCode
      structuralCheckCtxt.Code
      caseBodyTabs
      caseTabs
  ctxt.AddCode caseCode

//Each rule becomes a case in the switch statement.
let emitRulesCode (ctxt : CodeGenerationCtxt) =
  //the rules calling the current functions are the rules having one argument in the conclusion that
  //is the name of the current function.
  let tabs = emitTabs ctxt.CurrentTabs
  let rulesCallingCurrentFunction =
    ctxt.Program.TypedRules |>
    List.filter(fun rule ->
                  match rule with
                  | TypedRule(tr) ->
                      //The parser should ensure that the output of a rule is always a value and not a module.
                      let (ValueOutput(args,_)) = tr.Conclusion
                      args |> List.exists(fun arg ->
                                              match arg with
                                              | Id(id,_) -> id = ctxt.CurrentFuncId
                                              | _ -> false)
                  | TypedTypeRule _ -> false)
  let casesCode =
    rulesCallingCurrentFunction |>
    List.fold(fun (i,newCtxt) rule ->
                i + 1,
                emitRuleCase 
                  { newCtxt with
                      CurrentTabs = ctxt.CurrentTabs + 1 
                      RuleIndex = i}) (0,{ ctxt with RulesMatchingFunction = rulesCallingCurrentFunction.Length }) |> snd
  let switchCode =
    sprintf "%sswitch (__ruleIndex)\n%s{\n%s%s}"
      tabs
      tabs
      casesCode.Code
      tabs
  ctxt.AddCode(switchCode)

//Each function class contains a run method. The run defines a switch with a case for each
//rule calling the current function in its conclusion.
let emitRunMethod (ctxt : CodeGenerationCtxt) =
  let tabs = emitTabs ctxt.CurrentTabs
  let methodTabs = emitTabs (ctxt.CurrentTabs + 1)
  let switchCode =
    emitRulesCode
      { ctxt with
          Code = ""
          CurrentTabs = ctxt.CurrentTabs + 1 }
  let runCode =
    sprintf "%spublic void Run()\n%s{\n%sint __ruleIndex = 0;\n%sres = new %s<%s>();\n%s__res.Value = default(%s);\n%s__res.HasValue = false;\n%s\n%s}\n"
      tabs
      tabs
      methodTabs
      methodTabs
      resultStruct
      (getTypeFullName ctxt.CurrentRuleRetType)
      methodTabs
      (getTypeFullName ctxt.CurrentRuleRetType)
      methodTabs
      switchCode.Code
      tabs
  ctxt.AddCode runCode
  
//[[Func "f" -> arg1 -> ... -> argn : RetType ]]
//Create a class for each function declaration. The class has the name of the functiom
//and contains a field for each function argument. It also contains a field __res with type
//__MetaCnvResult<RetType> (compiler-generated) to store the result of the function evaluation.
let emitFunctionClasses (ctxt : CodeGenerationCtxt) =
  let tabs = emitTabs ctxt.CurrentTabs
  let classTabs = emitTabs (ctxt.CurrentTabs + 1)
  ctxt.Program.SymbolTable.FuncTable |>
  Map.fold(fun (newCtxt : CodeGenerationCtxt) id decl ->
              let argList =
                (extractTypeNamesFromTypeDecl decl.Args getTypeFullName)
              let argCode = emitArgCode argList classTabs
              let runCtxt = 
                emitRunMethod 
                  { newCtxt with
                      Code = "" 
                      CurrentTabs = newCtxt.CurrentTabs + 1
                      CurrentRuleRetType = decl.Return
                      CurrentFuncId = decl.Name }
              let classCode =
                sprintf "%spublic class %s\n%s{\n%s%spublic %s<%s> __res;\n%s%s}\n"
                  tabs
                  (renameOperator decl.Name.Name)
                  tabs
                  argCode
                  classTabs
                  resultStruct
                  (getTypeFullName decl.Return)
                  runCtxt.Code
                  tabs
              newCtxt.AddCode classCode) ctxt

let emitProgram (program : TypedProgramDefinition) = 
  let startingCtxt = CodeGenerationCtxt.Init(program)
  let ctxtWithInterfaces = emitDataInterfaces startingCtxt
  let ctxtWithDataClasses = emitDataClasses ctxtWithInterfaces
  let ctxtWithFunctionClasses = emitFunctionClasses ctxtWithDataClasses
  { startingCtxt with Code = startingCtxt.Code + (emitDataInterfaces startingCtxt).Code }
  
  


