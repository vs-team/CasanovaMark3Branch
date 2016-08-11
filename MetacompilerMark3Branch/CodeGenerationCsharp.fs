module CodeGenerationCsharp

open ParserAST
open TypeChecker
open Common
open DefaultMappings

type CodeGenerationCtxt =
  {
    Program           : TypedProgramDefinition
    Code              : string
    CurrentTabs       : int
    ArgIndex          : int
    RuleIndex         : int
  }
  static member Init(program : TypedProgramDefinition) = {Program = program; Code = ""; CurrentTabs = 0; ArgIndex = 0; RuleIndex = 0}

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
      | None -> id.ToString()
  | Arrow(t1,t2,_) ->
      "Func<" + (emitType t1) + "," + (emitType t2) + ">"
  | _ -> failwith "unsupported TypeDecl format in emitType"

let emitId (id : Id) (useNameSpace : bool) = 
  if useNameSpace then id.ToString() else id.Name

let emitReturnArg (ctxt : CodeGenerationCtxt) (retType : TypeDecl) =
  (emitTabs ctxt.CurrentTabs) + "public " + (emitType retType) + " __res" + ";\n"

let emitLocals (ctxt : CodeGenerationCtxt) (vars : Map<Id,TypeDecl * Position>) =
  Map.fold(fun code id (t,_) ->
                code + (emitTabs ctxt.CurrentTabs) + "public " + (emitType t) + " " + (emitId id false) + ";\n") "" vars

//let emitCheckCallStructure (ctxt : CodeGenerationCtxt) (args : CallArg list) =
//  List.fold (fun newCtxt arg ->
//              match arg with
//              | Id _ ->
//                  ctxt
//              | Literal(l,_) ->
//                  
//                ) ctxt args

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
                | Literal(l,_)->
                    code + tabs + "public " + (emitType t) + " __arg" + (string argIndex) + ";\n"
                | NestedExpression _ -> failwith "To be implemented"
                | CallArg.Lambda _ -> failwith "To be implemented"
                    ) "" args
      | _ -> failwith "Function name is not an id???"
    | _ -> failwith "No arguments in the left part of the conclusion????"
  | ModuleOutput _ -> failwith "Modules not supported yet"

let emitConclusion (ctxt : CodeGenerationCtxt) (conclusion : Conclusion) =
  let tabs = emitTabs ctxt.CurrentTabs
  match conclusion with
  | ValueOutput(call,res) ->
      match call with
      | [arg] ->
          { ctxt
              with
                Code = ctxt.Code +  tabs + "public void Run()\n{"(*+ emitConclusion ... *) + "\n}"
          }
      | fName :: args ->
          { ctxt
              with
                Code = ctxt.Code + tabs + "public void Run()\n{"(* + emitCheckCallStructure ... *) + "\n}"
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
  sprintf "%spublic class %s\n%s{\n%s%s%s%s}\n" tabs ("Rule" + (string ctxt.RuleIndex)) tabs locals nonVarArgs returnArg tabs

let emitRules (ctxt : CodeGenerationCtxt) : CodeGenerationCtxt =
  List.fold (fun newCtxt r -> 
                match r with
                | TypedRule tr ->
                    { newCtxt with 
                        Code = newCtxt.Code + (emitRule newCtxt tr)
                        RuleIndex = newCtxt.RuleIndex + 1
                    }
                | TypedTypeRule _ -> newCtxt) ctxt ctxt.Program.TypedRules

let defaultHeader =
  "using System;\n"

let emitDataStructures (ctxt : CodeGenerationCtxt) : string =
  let dataTable = ctxt.Program.SymbolTable.DataTable
  Map.fold(fun code data decl ->
              let subtypes = ctxt.Program.SymbolTable.Subtyping
              let tabs = emitTabs ctxt.CurrentTabs
              let retName =
                match decl.Return with
                | Arg(Id(id,_)) -> id.Name
                | _ -> failwith "The return argument of a Data structure should be an identifier..."
              if subtypes |> Map.exists(fun _ ts -> ts |> List.exists(fun t -> t === decl.Return)) then
                code + tabs + "public interface I" + retName + "{ }\n" //+ emitDataStructure ...
              else
                //PLACEHOLDER: emitDataStructure goes here
                code + "") "" dataTable

let emitProgram (program : TypedProgramDefinition) =
  let startingCtxt = CodeGenerationCtxt.Init program
  sprintf
    "%s\nnamespace %s\n {\n%s%s\n}"
    defaultHeader (program.Module) (emitDataStructures startingCtxt) ((emitRules startingCtxt).Code)