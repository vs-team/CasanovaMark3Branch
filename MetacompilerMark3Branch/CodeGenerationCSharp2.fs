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
    CurrentDataArgs           : string list
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
      CurrentDataArgs = []
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
  member this.AddCode (code : string) = { this with Code = this.Code + code }

let rec emitTabs tabs =
   if tabs = 0 then
    ""
   else
    "\t" + (emitTabs (tabs - 1))

let emitDataInterfaces (ctxt : CodeGenerationCtxt) =
  let tabs = emitTabs ctxt.CurrentTabs
  ctxt.Program.SymbolTable.DataTable |>
  Map.fold(fun newCtxt id decl ->
              match ctxt.Program.SymbolTable.Subtyping |> Map.tryFindKey(fun k _ -> k === decl.Return) with
              | None ->
                  { newCtxt with Code = newCtxt.Code + (sprintf "%spublic interface %s{ }\n" tabs (getTypeSimpleName decl.Return)) }
              | Some _type ->
                  let subtypes = ctxt.Program.SymbolTable.Subtyping.[_type]
                  let subtypesString =
                    subtypes |>
                    List.map(fun decl -> getTypeSimpleName decl) |>
                    List.reduce(fun x y -> x + ", " + y)
                  let interfaceString =
                    sprintf "%spublic interface %s{ } : %s\n" tabs (getTypeSimpleName decl.Return) subtypesString
                  { newCtxt with Code = newCtxt.Code + interfaceString }) ctxt

let emitDataClasses (ctxt : CodeGenerationCtxt) =
  let tabs = emitTabs ctxt.CurrentTabs
  let classTabs = emitTabs (ctxt.CurrentTabs + 1)
  let methodTabs = emitTabs (ctxt.CurrentTabs + 2)
  ctxt.Program.SymbolTable.DataTable |>
  Map.fold(fun (newCtxt : CodeGenerationCtxt) id decl ->
            let argList =
              (extractTypeNamesFromDataArg decl.Args getTypeFullName)
            let argCode =
              argList |>
              List.fold(fun (i,code) typeName ->
                          (i + 1,
                           code + (sprintf "%spublic %s __arg%d;\n" classTabs typeName i))) (0,"") |> snd
            let ToStringCode =
              let methodName = sprintf "%spublic override string ToString()\n" classTabs
              if argList.Length = 0 then
                sprintf "%s%s{\n%sreturn __name;\n%s}\n" methodName classTabs methodTabs classTabs
              else
                ""
            let classCode =
              sprintf "%spublic class %s\n%s{\n%spublic string name = \"%s\";\n%s%s}\n" 
                tabs 
                (renameOperator decl.Name.Name) 
                tabs 
                classTabs
                decl.Name.Name
                argCode
                tabs
            newCtxt.AddCode classCode) ctxt

let emitProgram (program : TypedProgramDefinition) = 
  let startingCtxt = CodeGenerationCtxt.Init(program)
  let ctxtWithInterfaces = emitDataInterfaces startingCtxt
  let ctxtWithClasses = emitDataClasses ctxtWithInterfaces
  { startingCtxt with Code = startingCtxt.Code + (emitDataInterfaces startingCtxt).Code }
  
  


