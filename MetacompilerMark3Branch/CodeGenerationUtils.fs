module CodeGenerationUtils

open ParserAST
open TypeChecker
open Common
open DefaultMappings

exception CodeGenerationError of string

let symbolUsedInSubtypes (decl : SymbolDeclaration) (subtypes : Map<TypeDecl,List<TypeDecl>>) =
  subtypes |> Map.exists(fun _ ts -> ts |> List.exists(fun t -> t === decl.Return))

let getTypeSimpleName (t : TypeDecl) =
  match t with
  | Arg(Id(id,_),_) -> id.Name
  | _ -> failwith "The return argument of a Data structure should be an identifier..."

let getTypeFullName (t : TypeDecl) =
  match t with
  | External(s,_) -> s
  | Unsafe -> "var"
  | Arg(Id(id,_),_) -> 
      if (id.Namespace = systemNamespace) then id.Name
      else (string id)
  | _ -> failwith "The return argument of a Data structure should be an identifier..."

let getReturnTypeSimpleName (decl : SymbolDeclaration) = getTypeSimpleName decl.Return

let getDeclInterface (decl : SymbolDeclaration) =
  getReturnTypeSimpleName decl

let rec extractTypeNamesFromTypeDecl (_type : TypeDecl) (nameFunction : TypeDecl -> string) : string list =
  match _type with
  | Arrow(left,right,_) ->
      (string left) :: (extractTypeNamesFromTypeDecl right nameFunction)
  | Arg _ -> [nameFunction _type]
  | External(s,_) -> [s]
  | Zero -> []

let getSymbolFullName (decl : SymbolDeclaration) =
  (decl.Name.Namespace) + "." + (renameOperator decl.Name.Name)


