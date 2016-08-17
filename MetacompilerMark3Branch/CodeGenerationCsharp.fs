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
    CurrentRuleRetType        : TypeDecl
  }
  static member Init(program : TypedProgramDefinition) = 
    {
      Program = program; Code = ""
      CurrentTabs = 0
      ArgIndex = 0
      RuleIndex = 0
      CurrentRuleRetType = Zero
    }

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


let emitReflectionStructuralCheck (ctxt : CodeGenerationCtxt) (dataSymbol : SymbolDeclaration) (index : int) (superArgName : string) =
  let tabs = emitTabs ctxt.CurrentTabs
  (sprintf "\n%sif (!(%s is %s)) \n%s{\n%s _res = new %s<%s>();\n%sreturn;\n%s}" 
    tabs 
    superArgName
    (renameOperator dataSymbol.Name.Name )
    tabs
    (emitTabs (ctxt.CurrentTabs + 1))
    resultNone
    (emitType ctxt.CurrentRuleRetType)
    (emitTabs (ctxt.CurrentTabs + 1))
    tabs)

let rec emitStructuralCheck (ctxt : CodeGenerationCtxt) (args : CallArg list) (superArgName : string) =
  args |>
  List.fold(fun (code,index) arg ->
              let tabs = emitTabs ctxt.CurrentTabs
              let composedArg = superArgName + (if superArgName <> "" then "." else "") + "__arg" + (string index)
              match arg with
              | Literal(l,_) ->
                  code + 
                   (sprintf "\n%sif (%s != %s)\n%s{\n%s __res = new %s<%s>();\n%sreturn;\n%s}" 
                   tabs 
                   composedArg
                   (l.ToString()) 
                   tabs 
                   (emitTabs (ctxt.CurrentTabs + 1))
                   resultNone
                   (emitType ctxt.CurrentRuleRetType) 
                   (emitTabs (ctxt.CurrentTabs + 1)) tabs),index + 1
              | NestedExpression nestedArgs ->
                  let dataName = nestedArgs.Head
                  let arguments = nestedArgs.Tail
                  match dataName with
                  | Id(id,_) ->
                    let dataSymbol = ctxt.Program.SymbolTable.DataTable.[id]
                    code + (emitReflectionStructuralCheck ctxt dataSymbol index composedArg) + (fst (emitStructuralCheck ctxt nestedArgs composedArg)), index + 1
                  | _ -> failwith "Data name is not an id???"
              //if you have a constructor with zero arguments it is an id, so remember to put here the code to check if the structure is correct if the id is a data constructor.
              | Id(id,_) ->
                  match ctxt.Program.SymbolTable.DataTable |> Map.tryFind id with
                  | Some symbol -> code + (emitReflectionStructuralCheck ctxt symbol index composedArg),(index + 1)
                  | None -> code,(index + 1)
              | CallArg.Lambda _ -> code,(index + 1)) ("",0)

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
          { ctxt
              with
                //TODO: emit the code to check the structural equality if there are Data constructor arguments or literal and the code for premises inside Run
                Code = ctxt.Code + tabs + "public void Run()\n" + tabs + "{" + (fst (emitStructuralCheck {ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } args "")) + "\n" + tabs + "}\n"
          }
  | ModuleOutput _ -> failwith "Module generation not supported yet"

let emitRule (ctxt : CodeGenerationCtxt) (rule : TypedRule) =
  let tabs = emitTabs ctxt.CurrentTabs
  let locals = emitLocals { ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } rule.Locals.Variables
  let nonVarArgs =
    match rule.Conclusion with
    | ValueOutput(call,res) ->
        emitNonVariableArgs { ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } rule.Conclusion
    | ModuleOutput _ -> failwith "Modules not supported yet"
  let returnArg =  emitReturnArg { ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } rule.ReturnType
  let check = (emitConclusionCheck { ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } rule.Conclusion).Code
  sprintf "%spublic class %s\n%s{\n%s%s%s%s%s}\n" tabs ("Rule" + (string ctxt.RuleIndex)) tabs locals nonVarArgs returnArg check tabs

let emitRules (ctxt : CodeGenerationCtxt) : CodeGenerationCtxt =
  List.fold (fun newCtxt r -> 
                match r with
                | TypedRule tr ->
                    { newCtxt with 
                        Code = newCtxt.Code + (emitRule { newCtxt with CurrentRuleRetType = tr.ReturnType } tr)
                        RuleIndex = newCtxt.RuleIndex + 1
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