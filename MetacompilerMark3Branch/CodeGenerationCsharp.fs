module CodeGenerationCsharp

open ParserAST
open TypeChecker
open Common
open DefaultMappings

let resultInterface = "__MetaCnvResult"
let resultValue = "__MetaCnvValue"
let resultNone = "__MetaCnvNone"

type CodeGenerationCtxt =
  {
    Program                   : TypedProgramDefinition
    Code                      : string
    CurrentTabs               : int
    ArgIndex                  : int
    RuleIndex                 : int
    TempIndex                 : int
    GeneratedTemps            : string list
    CurrentRuleRetType        : TypeDecl
  }
  static member Init(program : TypedProgramDefinition) = 
    {
      Program = program; Code = ""
      CurrentTabs = 0
      ArgIndex = 0
      RuleIndex = 0
      TempIndex = 0
      GeneratedTemps = []
      CurrentRuleRetType = Zero
    }
  member private this.TempName index = "__tmp" + (string index)
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

let symbolUsedInSubtypes (decl : SymbolDeclaration) (subtypes : Map<TypeDecl,List<TypeDecl>>) =
  subtypes |> Map.exists(fun _ ts -> ts |> List.exists(fun t -> t === decl.Return))

let getTypeSimpleName (t : TypeDecl) =
  match t with
  | Arg(Id(id,_)) -> id.Name
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
  | Arg(Id(id,_)) ->
      let defaultMapping = typeMappingsCsharp |> Map.tryFind (Arg(Id(id,emptyPos)))
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
  (emitTabs ctxt.CurrentTabs) + "public " +  resultInterface + "<" + 
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

let composeArgPath (ctxt : CodeGenerationCtxt) (argIndex : int) =
  (if ctxt.GeneratedTemps.Length = 0 then "" else ctxt.LastTempCode + ".") + "__arg" + (string argIndex)


let emitReflectionStructuralCheck (ctxt : CodeGenerationCtxt) (dataSymbol : SymbolDeclaration) (index : int) =
  let tabs = emitTabs ctxt.CurrentTabs
  let fullName = composeArgPath ctxt index
  let opName = renameOperator dataSymbol.Name.Name
  let updatedCtxt = ctxt.AddTemp
  (sprintf "\n%sif (!(%s is %s)) \n%s{\n%s _res = new %s<%s>();\n%sreturn;\n%s}\n%s%s %s = (%s)%s;" 
    tabs 
    fullName
    (renameOperator dataSymbol.Name.Name )
    tabs
    (emitTabs (ctxt.CurrentTabs + 1))
    resultNone
    (emitType ctxt.CurrentRuleRetType)
    (emitTabs (ctxt.CurrentTabs + 1))
    tabs
    tabs
    opName
    ctxt.CurrentTempCode
    opName
    fullName),updatedCtxt

let rec emitStructuralCheck (ctxt : CodeGenerationCtxt) (args : CallArg list) =
  args |>
  List.fold(fun (code,index,newCtxt) arg ->
              let tabs = emitTabs newCtxt.CurrentTabs
              let composedArg = composeArgPath ctxt index
              match arg with
              | Literal(l,_) ->
                  code + 
                   (sprintf "\n%sif (%s != %s)\n%s{\n%s __res = new %s<%s>();\n%sreturn;\n%s}" 
                   tabs 
                   composedArg
                   (l.ToString()) 
                   tabs 
                   (emitTabs (newCtxt.CurrentTabs + 1))
                   resultNone
                   (emitType newCtxt.CurrentRuleRetType) 
                   (emitTabs (newCtxt.CurrentTabs + 1)) tabs),index + 1,newCtxt
              | NestedExpression nestedArgs ->
                  let dataName = nestedArgs.Head
                  let arguments = nestedArgs.Tail
                  match dataName with
                  | Id(id,_) ->
                    let dataSymbol = newCtxt.Program.SymbolTable.DataTable.[id]
                    let checkCode,updatedCtxt = emitReflectionStructuralCheck newCtxt dataSymbol index
                    let structCode,_,nestedCtxt = emitStructuralCheck updatedCtxt arguments
                    code + checkCode + structCode,index + 1,{ updatedCtxt with TempIndex = nestedCtxt.TempIndex; GeneratedTemps = nestedCtxt.GeneratedTemps }
                  | _ -> failwith "Data name is not an id???"
              //if you have a constructor with zero arguments it is an id, so remember to put here the code to check if the structure is correct if the id is a data constructor.
              | Id(id,_) ->
                  match newCtxt.Program.SymbolTable.DataTable |> Map.tryFind id with
                  | Some symbol when symbol.Args = Zero ->
                      let checkCode,updatedCtxt = emitReflectionStructuralCheck newCtxt symbol index
                      code + checkCode,(index + 1),updatedCtxt
                  | _ -> code,(index + 1),newCtxt
              | CallArg.Lambda _ -> code,(index + 1),newCtxt) ("",0,ctxt)

let emitConclusionCheck (ctxt : CodeGenerationCtxt) (conclusion : Conclusion) =
  let tabs = emitTabs ctxt.CurrentTabs
  match conclusion with
  | ValueOutput(call,res) ->
      match call with
      | [arg] ->
          { ctxt
              with
                //TODO: emit of the premises inside Run
                Code = ctxt.Code +  tabs + "public void Run()\n" + tabs + "{" + "\n" + tabs + "}\n"
          }
      | fName :: args ->
          let checkCode,_,updatedCtxt = emitStructuralCheck {ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } args
          { ctxt
              with
                //TODO: emit the code to check the structural equality if there are Data constructor arguments or literal and the code for premises inside Run
                Code = ctxt.Code + tabs + "public void Run()\n" + tabs + "{" + checkCode + "\n" + tabs + "}\n"
                TempIndex = updatedCtxt.TempIndex
                GeneratedTemps = updatedCtxt.GeneratedTemps
          }
      | _ -> failwith "Call is empty?!!"
  | ModuleOutput _ -> failwith "Module generation not supported yet"

let emitCallArgumentsCopy (ctxt : CodeGenerationCtxt) (args : CallArg list) (rules : TypedRuleDefinition list) =
  ctxt
  
// When implementing lambdas, here you should check if the argument is among the local variables and if it is a lambda. If that is the case then just call the lambda.
let emitFunctionCall (ctxt : CodeGenerationCtxt) (functionCall : CallArg list * CallArg list) =
  let call,ret = functionCall
  let matchingRules = ctxt.Program.TypedRules |> 
                      List.filter(fun ruleDef ->
                                    match ruleDef with
                                    | TypedRule(tr) -> 
                                        match tr.Conclusion with
                                        | ValueOutput(c,_) ->
                                            match call.Head,c.Head with
                                            | Id(id1,_),Id(id2,_) -> id1.Name = id2.Name
                                            | _ -> failwith "The first argument is not an id??!!"
                                        | ModuleOutput _ -> failwith "Module generation not supported yet"
                                    | TypedTypeRule(tr) -> failwith "Type Rules not supported yet")
  call |> List.fold(fun newCtxt c -> emitCallArgumentsCopy newCtxt call matchingRules) ctxt

let emitPremises (ctxt : CodeGenerationCtxt) (premises : Premise list) =
  premises |>
  List.fold (fun newCtxt p ->
                match p with
                | FunctionCall(args,res) -> emitFunctionCall ctxt (args,res)
                | Bind _
                | Conditional _ -> failwith "Not implemented yet...") ctxt

let emitRule (ctxt : CodeGenerationCtxt) (rule : TypedRule) =
  let tabs = emitTabs ctxt.CurrentTabs
  let locals = emitLocals { ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } rule.Locals.Variables
  let nonVarArgs =
    match rule.Conclusion with
    | ValueOutput(call,res) ->
        emitNonVariableArgs { ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } rule.Conclusion
    | ModuleOutput _ -> failwith "Modules not supported yet"
  let returnArg =  emitReturnArg { ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } rule.ReturnType
  let conclusionCtxt = emitConclusionCheck { ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } rule.Conclusion
  let premiseCtxt = emitPremises conclusionCtxt rule.Premises
  let check = conclusionCtxt.Code
  sprintf "%spublic class %s\n%s{\n%s%s%s%s%s}\n" tabs ("Rule" + (string ctxt.RuleIndex)) tabs locals nonVarArgs returnArg check tabs

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
   public interface " +  resultInterface  + "<T> {  }\n
   public class " + resultValue + "<T> : " + resultInterface + "<T> { public T Value; }\n
   public class " + resultNone + "<T> : " + resultInterface + "<T>  { }\n
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
  | Arg(Id(_)) -> tabs + "public " + (emitType t) + " __arg" + (string currentIndex) + ";\n"
  | Zero -> ""
  | _ -> failwith "Invalid type format in data declaration..."
  

let emitDataStructure (ctxt : CodeGenerationCtxt) (decl : SymbolDeclaration) : string =
  let tabs = emitTabs ctxt.CurrentTabs
  let subtypes = ctxt.Program.SymbolTable.Subtyping
  let interfaces =
    getDeclInterface decl +
    (match subtypes |> Map.tryFindKey (fun t _ -> t === decl.Return) with
     | Some _ ->
        let types = subtypes.[subtypes |> Map.findKey (fun t _ -> t === decl.Return)]
        if types.Length = 0 then "" 
        else 
          "," +
          (types |> 
           List.map(fun t -> getTypeSimpleName t) |>
           List.reduce(fun t1 t2 ->
                              t1 + "," + t2))
     | None -> "")
  "public class " + (renameOperator decl.Name.Name) + (if interfaces <> "" then " : " + interfaces else "") + "\n" + tabs + "{\n" + (emitDataArgs {ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } decl.Args 0) + tabs + "}\n"

let emitDataStructures (ctxt : CodeGenerationCtxt) : string =
  let dataTable = ctxt.Program.SymbolTable.DataTable
  Map.fold(fun code data decl ->
              let subtypes = ctxt.Program.SymbolTable.Subtyping
              let tabs = emitTabs ctxt.CurrentTabs
              let retName = getReturnTypeSimpleName decl
              code + tabs + "public interface " + retName + "{ }\n" + tabs + emitDataStructure ctxt decl) "" dataTable

let emitProgram (program : TypedProgramDefinition) =
  let startingCtxt = CodeGenerationCtxt.Init program
  sprintf
    "%s\nnamespace %s\n {\n%s%s%s\n}"
    defaultHeader (program.Module) defaultClasses (emitDataStructures startingCtxt) ((emitRules startingCtxt).Code)