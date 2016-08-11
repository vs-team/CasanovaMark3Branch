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

let emitId (id : Id) (useNameSpace : bool) = 
  if useNameSpace then id.ToString() else id.Name

let emitReturnArg (ctxt : CodeGenerationCtxt) (retType : TypeDecl) =
  (emitTabs ctxt.CurrentTabs) + "public " + (emitType retType) + " __res" + ";\n"

let emitLocals (ctxt : CodeGenerationCtxt) (vars : Map<Id,TypeDecl * Position>) =
  Map.fold(fun code id (t,_) ->
                code + (emitTabs ctxt.CurrentTabs) + "public " + (emitType t) + " " + (emitId id false) + ";\n") "" vars

//let emitArg (ctxt : CodeGenerationCtxt) (arg : CallArg) (locals : LocalContext) =
//  match arg with
//  | Literal(l,_) -> (string l)
//  | Id(id,_) -> 
//  | NestedExpression _ -> "Nested Expression code generation not implemented yet"
//  | CallArg.Lambda _-> "Lambda code generation not implemented yet"
//
//let emitArgs (ctxt : CodeGenerationCtxt) (args : CallArg list) (locals : LocalContext) =
//  match args with
//  | [Id(fName,_)] ->  
//
//let emitPremise (ctxt : CodeGenerationCtxt) (premise : Premise) (locals : LocalContext) =
//  match premise with
//  | FunctionCall(call,res) ->
//      List.fold (fun newCtxt arg ->
//                    { newCtxt
//                        with
//                          Code = newCtxt.Code + (emitArg ctxt arg locals)
//                    }) ctxt call
//  | Bind _ -> failwith "Bind code generation not implemented yet"
//  | Conditional _ -> failwith "Conditional code generation not implemented yet"

let emitRule (ctxt : CodeGenerationCtxt) (rule : TypedRule) =
  let tabs = emitTabs ctxt.CurrentTabs
  let locals = emitLocals { ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } rule.Locals.Variables
  let returnArg =  emitReturnArg { ctxt with CurrentTabs = ctxt.CurrentTabs + 1 } rule.ReturnType
  sprintf "%spublic class %s\n%s{\n%s%s%s}\n" tabs ("Rule" + (string ctxt.RuleIndex)) tabs locals returnArg tabs

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

let emitProgram (program : TypedProgramDefinition) =
  sprintf
    "%s\nnamespace %s\n {\n%s\n}"
    defaultHeader (program.Module) ((emitRules {Program = program; Code = ""; CurrentTabs = 0; ArgIndex = 0; RuleIndex = 0}).Code)