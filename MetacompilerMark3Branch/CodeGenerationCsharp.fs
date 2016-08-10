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

let emitId (id : Id) = id.Name
  

let emitLocals (ctxt : CodeGenerationCtxt) (vars : Map<Id,TypeDecl * Position>) =
  Map.fold(fun code id (t,_) ->
                code + (emitTabs ctxt.CurrentTabs) + "public " + (emitType t) + " " + (emitId id) + ";\n") "" vars
let emitRule (ctxt : CodeGenerationCtxt) (rule : TypedRule) =
  let tabs = emitTabs ctxt.CurrentTabs
  let locals = emitLocals { ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } rule.Locals.Variables
  sprintf "%sclass %s\n%s{\n%s%s}\n" tabs ("Rule" + (string ctxt.RuleIndex)) tabs locals tabs

let emitRules (ctxt : CodeGenerationCtxt) : CodeGenerationCtxt =
  List.fold (fun newCtxt r -> 
                match r with
                | TypedRule tr ->
                    let res =
                      { newCtxt with 
                          Code = newCtxt.Code + (emitRule newCtxt tr)
                          RuleIndex = newCtxt.RuleIndex + 1
                      }
                    res
                | TypedTypeRule _ -> newCtxt) ctxt ctxt.Program.TypedRules

let emitProgram (program : TypedProgramDefinition) =
  sprintf
    "using system;\nnamespace %s\n {\n%s\n}"
    (program.Module) ((emitRules {Program = program; Code = ""; CurrentTabs = 0; ArgIndex = 0; RuleIndex = 0}).Code)