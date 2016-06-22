// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "Parser.fsy"


open Common
open ParserAST
open System
open ParserUtils


# 15 "Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | FUNC of (int * int)
  | TYPEFUNC of (int * int)
  | OPEN of (int * int)
  | INCLUDE of (int * int)
  | DATA of (int * int)
  | MODULE of (int * int)
  | COMMA
  | LBRACKET
  | RBRACKET
  | DOT
  | APOSTROPHE
  | HASH
  | COLON
  | ARROW of (System.Int32 * System.Int32)
  | DARROW of (System.Int32 * System.Int32)
  | EQUAL of (System.Int32 * System.Int32)
  | STRING of (System.String)
  | ID of (System.String)
  | FLOAT of (System.Double)
  | INT of (System.Int32)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_FUNC
    | TOKEN_TYPEFUNC
    | TOKEN_OPEN
    | TOKEN_INCLUDE
    | TOKEN_DATA
    | TOKEN_MODULE
    | TOKEN_COMMA
    | TOKEN_LBRACKET
    | TOKEN_RBRACKET
    | TOKEN_DOT
    | TOKEN_APOSTROPHE
    | TOKEN_HASH
    | TOKEN_COLON
    | TOKEN_ARROW
    | TOKEN_DARROW
    | TOKEN_EQUAL
    | TOKEN_STRING
    | TOKEN_ID
    | TOKEN_FLOAT
    | TOKEN_INT
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_program
    | NONTERM_dottedPath
    | NONTERM_includeOrOpenStmts
    | NONTERM_includeOrOpenStmt
    | NONTERM_declarations
    | NONTERM_declaration
    | NONTERM_typeOrNameDeclarations
    | NONTERM_typeOrNameDeclaration
    | NONTERM_typeDeclaration
    | NONTERM_lambdaTypeDeclaration

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | FUNC _ -> 1 
  | TYPEFUNC _ -> 2 
  | OPEN _ -> 3 
  | INCLUDE _ -> 4 
  | DATA _ -> 5 
  | MODULE _ -> 6 
  | COMMA  -> 7 
  | LBRACKET  -> 8 
  | RBRACKET  -> 9 
  | DOT  -> 10 
  | APOSTROPHE  -> 11 
  | HASH  -> 12 
  | COLON  -> 13 
  | ARROW _ -> 14 
  | DARROW _ -> 15 
  | EQUAL _ -> 16 
  | STRING _ -> 17 
  | ID _ -> 18 
  | FLOAT _ -> 19 
  | INT _ -> 20 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_FUNC 
  | 2 -> TOKEN_TYPEFUNC 
  | 3 -> TOKEN_OPEN 
  | 4 -> TOKEN_INCLUDE 
  | 5 -> TOKEN_DATA 
  | 6 -> TOKEN_MODULE 
  | 7 -> TOKEN_COMMA 
  | 8 -> TOKEN_LBRACKET 
  | 9 -> TOKEN_RBRACKET 
  | 10 -> TOKEN_DOT 
  | 11 -> TOKEN_APOSTROPHE 
  | 12 -> TOKEN_HASH 
  | 13 -> TOKEN_COLON 
  | 14 -> TOKEN_ARROW 
  | 15 -> TOKEN_DARROW 
  | 16 -> TOKEN_EQUAL 
  | 17 -> TOKEN_STRING 
  | 18 -> TOKEN_ID 
  | 19 -> TOKEN_FLOAT 
  | 20 -> TOKEN_INT 
  | 23 -> TOKEN_end_of_input
  | 21 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_program 
    | 3 -> NONTERM_dottedPath 
    | 4 -> NONTERM_dottedPath 
    | 5 -> NONTERM_includeOrOpenStmts 
    | 6 -> NONTERM_includeOrOpenStmts 
    | 7 -> NONTERM_includeOrOpenStmt 
    | 8 -> NONTERM_includeOrOpenStmt 
    | 9 -> NONTERM_declarations 
    | 10 -> NONTERM_declarations 
    | 11 -> NONTERM_declaration 
    | 12 -> NONTERM_declaration 
    | 13 -> NONTERM_typeOrNameDeclarations 
    | 14 -> NONTERM_typeOrNameDeclarations 
    | 15 -> NONTERM_typeOrNameDeclarations 
    | 16 -> NONTERM_typeOrNameDeclaration 
    | 17 -> NONTERM_typeOrNameDeclaration 
    | 18 -> NONTERM_typeDeclaration 
    | 19 -> NONTERM_typeDeclaration 
    | 20 -> NONTERM_typeDeclaration 
    | 21 -> NONTERM_lambdaTypeDeclaration 
    | 22 -> NONTERM_lambdaTypeDeclaration 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 23 
let _fsyacc_tagOfErrorTerminal = 21

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | FUNC _ -> "FUNC" 
  | TYPEFUNC _ -> "TYPEFUNC" 
  | OPEN _ -> "OPEN" 
  | INCLUDE _ -> "INCLUDE" 
  | DATA _ -> "DATA" 
  | MODULE _ -> "MODULE" 
  | COMMA  -> "COMMA" 
  | LBRACKET  -> "LBRACKET" 
  | RBRACKET  -> "RBRACKET" 
  | DOT  -> "DOT" 
  | APOSTROPHE  -> "APOSTROPHE" 
  | HASH  -> "HASH" 
  | COLON  -> "COLON" 
  | ARROW _ -> "ARROW" 
  | DARROW _ -> "DARROW" 
  | EQUAL _ -> "EQUAL" 
  | STRING _ -> "STRING" 
  | ID _ -> "ID" 
  | FLOAT _ -> "FLOAT" 
  | INT _ -> "INT" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | FUNC _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | TYPEFUNC _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | OPEN _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | INCLUDE _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | DATA _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | MODULE _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | COMMA  -> (null : System.Object) 
  | LBRACKET  -> (null : System.Object) 
  | RBRACKET  -> (null : System.Object) 
  | DOT  -> (null : System.Object) 
  | APOSTROPHE  -> (null : System.Object) 
  | HASH  -> (null : System.Object) 
  | COLON  -> (null : System.Object) 
  | ARROW _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | DARROW _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | EQUAL _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | STRING _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | ID _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | FLOAT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | INT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 1us; 65535us; 0us; 2us; 4us; 65535us; 4us; 5us; 9us; 10us; 13us; 14us; 15us; 16us; 2us; 65535us; 5us; 6us; 11us; 12us; 2us; 65535us; 5us; 11us; 11us; 11us; 2us; 65535us; 6us; 7us; 17us; 18us; 2us; 65535us; 6us; 17us; 17us; 17us; 3us; 65535us; 19us; 20us; 23us; 24us; 28us; 29us; 3us; 65535us; 19us; 27us; 23us; 27us; 28us; 27us; 7us; 65535us; 19us; 31us; 21us; 22us; 23us; 31us; 25us; 26us; 28us; 31us; 32us; 38us; 39us; 38us; 2us; 65535us; 32us; 33us; 39us; 40us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 5us; 10us; 13us; 16us; 19us; 22us; 26us; 30us; 38us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 1us; 1us; 2us; 1us; 2us; 1us; 2us; 1us; 2us; 2us; 3us; 4us; 1us; 3us; 1us; 3us; 1us; 5us; 1us; 5us; 1us; 7us; 1us; 7us; 1us; 8us; 1us; 8us; 1us; 9us; 1us; 9us; 1us; 11us; 1us; 11us; 1us; 11us; 1us; 11us; 1us; 12us; 1us; 12us; 1us; 12us; 1us; 12us; 2us; 13us; 14us; 1us; 13us; 1us; 13us; 1us; 16us; 1us; 17us; 1us; 18us; 1us; 18us; 1us; 18us; 1us; 19us; 1us; 19us; 1us; 20us; 2us; 21us; 22us; 1us; 21us; 1us; 21us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 10us; 12us; 14us; 16us; 19us; 21us; 23us; 25us; 27us; 29us; 31us; 33us; 35us; 37us; 39us; 41us; 43us; 45us; 47us; 49us; 51us; 53us; 55us; 58us; 60us; 62us; 64us; 66us; 68us; 70us; 72us; 74us; 76us; 78us; 81us; 83us; |]
let _fsyacc_action_rows = 41
let _fsyacc_actionTableElements = [|1us; 32768us; 6us; 4us; 0us; 49152us; 1us; 32768us; 0us; 3us; 0us; 16385us; 1us; 32768us; 18us; 8us; 2us; 16390us; 3us; 15us; 4us; 13us; 2us; 16394us; 1us; 19us; 5us; 23us; 0us; 16386us; 1us; 16388us; 10us; 9us; 1us; 32768us; 18us; 8us; 0us; 16387us; 2us; 16390us; 3us; 15us; 4us; 13us; 0us; 16389us; 1us; 32768us; 18us; 8us; 0us; 16391us; 1us; 32768us; 18us; 8us; 0us; 16392us; 2us; 16394us; 1us; 19us; 5us; 23us; 0us; 16393us; 4us; 16399us; 8us; 32us; 11us; 35us; 17us; 30us; 18us; 37us; 1us; 32768us; 15us; 21us; 3us; 32768us; 8us; 32us; 11us; 35us; 18us; 37us; 0us; 16395us; 4us; 16399us; 8us; 32us; 11us; 35us; 17us; 30us; 18us; 37us; 1us; 32768us; 15us; 25us; 3us; 32768us; 8us; 32us; 11us; 35us; 18us; 37us; 0us; 16396us; 1us; 16398us; 14us; 28us; 4us; 16399us; 8us; 32us; 11us; 35us; 17us; 30us; 18us; 37us; 0us; 16397us; 0us; 16400us; 0us; 16401us; 3us; 32768us; 8us; 32us; 11us; 35us; 18us; 37us; 1us; 32768us; 9us; 34us; 0us; 16402us; 1us; 32768us; 18us; 36us; 0us; 16403us; 0us; 16404us; 1us; 16406us; 14us; 39us; 3us; 32768us; 8us; 32us; 11us; 35us; 18us; 37us; 0us; 16405us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 2us; 3us; 5us; 6us; 8us; 11us; 14us; 15us; 17us; 19us; 20us; 23us; 24us; 26us; 27us; 29us; 30us; 33us; 34us; 39us; 41us; 45us; 46us; 51us; 53us; 57us; 58us; 60us; 65us; 66us; 67us; 68us; 72us; 74us; 75us; 77us; 78us; 79us; 81us; 85us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 4us; 3us; 1us; 2us; 0us; 2us; 2us; 2us; 0us; 4us; 4us; 3us; 1us; 0us; 1us; 1us; 3us; 2us; 1us; 3us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 3us; 3us; 4us; 4us; 5us; 5us; 6us; 6us; 7us; 7us; 8us; 8us; 8us; 9us; 9us; 10us; 10us; 10us; 11us; 11us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 65535us; 65535us; 16386us; 65535us; 65535us; 16387us; 65535us; 16389us; 65535us; 16391us; 65535us; 16392us; 65535us; 16393us; 65535us; 65535us; 65535us; 16395us; 65535us; 65535us; 65535us; 16396us; 65535us; 65535us; 16397us; 16400us; 16401us; 65535us; 65535us; 16402us; 65535us; 16403us; 16404us; 65535us; 65535us; 16405us; |]
let _fsyacc_reductions ()  =    [| 
# 223 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data :  ParserAST.Program )) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 232 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'program)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 32 "Parser.fsy"
                                          _1 
                   )
# 32 "Parser.fsy"
                 :  ParserAST.Program ));
# 243 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int * int)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'dottedPath)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'includeOrOpenStmts)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'declarations)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 34 "Parser.fsy"
                                                                                 
                       _2,_3,(_4,[],[]) 
                   )
# 34 "Parser.fsy"
                 : 'program));
# 258 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : System.String)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'dottedPath)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "Parser.fsy"
                                           _1 + "." + _3 
                   )
# 38 "Parser.fsy"
                 : 'dottedPath));
# 270 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : System.String)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "Parser.fsy"
                                              _1 
                   )
# 39 "Parser.fsy"
                 : 'dottedPath));
# 281 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'includeOrOpenStmt)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'includeOrOpenStmts)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 42 "Parser.fsy"
                                                              _1 :: _2 
                   )
# 42 "Parser.fsy"
                 : 'includeOrOpenStmts));
# 293 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "Parser.fsy"
                         [] 
                   )
# 43 "Parser.fsy"
                 : 'includeOrOpenStmts));
# 303 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int * int)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'dottedPath)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 46 "Parser.fsy"
                                            _2 
                   )
# 46 "Parser.fsy"
                 : 'includeOrOpenStmt));
# 315 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int * int)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'dottedPath)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 47 "Parser.fsy"
                                         _2 
                   )
# 47 "Parser.fsy"
                 : 'includeOrOpenStmt));
# 327 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'declaration)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'declarations)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "Parser.fsy"
                                                  
                       printfn "Declaration found"
                       _1 :: _2 
                   )
# 50 "Parser.fsy"
                 : 'declarations));
# 341 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 53 "Parser.fsy"
                         [] 
                   )
# 53 "Parser.fsy"
                 : 'declarations));
# 351 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int * int)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'typeOrNameDeclarations)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : System.Int32 * System.Int32)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'typeDeclaration)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "Parser.fsy"
                                                                           
                         printfn "Function found"
                         Func(processParsedArgs _2 _4 (fst _1) (snd _1)) 
                   )
# 56 "Parser.fsy"
                 : 'declaration));
# 367 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int * int)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'typeOrNameDeclarations)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : System.Int32 * System.Int32)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'typeDeclaration)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 59 "Parser.fsy"
                                                                           
                         printfn "Data found"
                         Data(processParsedArgs _2 _4 (fst _1) (snd _1)) 
                   )
# 59 "Parser.fsy"
                 : 'declaration));
# 383 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'typeOrNameDeclaration)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : System.Int32 * System.Int32)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'typeOrNameDeclarations)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "Parser.fsy"
                                                                            _1 :: _3 
                   )
# 64 "Parser.fsy"
                 : 'typeOrNameDeclarations));
# 396 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'typeOrNameDeclaration)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 65 "Parser.fsy"
                                               [_1] 
                   )
# 65 "Parser.fsy"
                 : 'typeOrNameDeclarations));
# 407 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 66 "Parser.fsy"
                         [] 
                   )
# 66 "Parser.fsy"
                 : 'typeOrNameDeclarations));
# 417 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : System.String)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 69 "Parser.fsy"
                                ParserUtils.Name(_1) 
                   )
# 69 "Parser.fsy"
                 : 'typeOrNameDeclaration));
# 428 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'typeDeclaration)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 70 "Parser.fsy"
                                         ParserUtils.Type(_1) 
                   )
# 70 "Parser.fsy"
                 : 'typeOrNameDeclaration));
# 439 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'lambdaTypeDeclaration)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 74 "Parser.fsy"
                                                                 printfn "lambda found"; _2 
                   )
# 74 "Parser.fsy"
                 : 'typeDeclaration));
# 450 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : System.String)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 75 "Parser.fsy"
                                       printfn "Generic found"; Generic({ Namespace = ""; Name = _2 })
                   )
# 75 "Parser.fsy"
                 : 'typeDeclaration));
# 461 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : System.String)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 76 "Parser.fsy"
                            sprintf "Type found %s" _1; Arg(Id({ Namespace = ""; Name = _1 },emptyPos)) 
                   )
# 76 "Parser.fsy"
                 : 'typeDeclaration));
# 472 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'typeDeclaration)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : System.Int32 * System.Int32)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'lambdaTypeDeclaration)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 79 "Parser.fsy"
                                                                     printfn "Arrow found"; Arrow(_1,_3) 
                   )
# 79 "Parser.fsy"
                 : 'lambdaTypeDeclaration));
# 485 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'typeDeclaration)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 80 "Parser.fsy"
                                         _1 
                   )
# 80 "Parser.fsy"
                 : 'lambdaTypeDeclaration));
|]
# 497 "Parser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 24;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf :  ParserAST.Program  =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
