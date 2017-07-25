// Signature file for parser generated by fsyacc
module Parser
type token = 
  | EOF
  | FUNC of (int * int)
  | TYPEFUNC of (int * int)
  | OPEN of (int * int)
  | INCLUDE of (int * int)
  | DATA of (int * int)
  | NAMESPACE of (int * int)
  | IS of (int * int)
  | FUN of (int * int)
  | GT of (int * int)
  | GEQ of (int * int)
  | LT of (int * int)
  | LEQ of (int * int)
  | NEQ of (int * int)
  | MAIN of (int * int)
  | ARITHMETIC of (int * int)
  | MODULE of (int * int)
  | PRIORITY of (int * int)
  | LEMIT of (int * int)
  | REMIT of (int * int)
  | COMMA
  | LBRACKET
  | RBRACKET
  | DOT
  | APOSTROPHE
  | HASH
  | COLON
  | NEWLINE
  | BIND
  | LBRACE
  | RBRACE
  | LSQUARE
  | RSQUARE
  | BAR of (int)
  | ARROW of (System.Int32 * System.Int32)
  | DARROW of (System.Int32 * System.Int32)
  | EQUAL of (System.Int32 * System.Int32)
  | EMIT of (string * (int * int))
  | UNIT of ((int * int))
  | STRING of (string * (int * int))
  | ID of (System.String * (int * int))
  | FLOAT of (float * (int * int))
  | INT of (int * (int * int))
type tokenId = 
    | TOKEN_EOF
    | TOKEN_FUNC
    | TOKEN_TYPEFUNC
    | TOKEN_OPEN
    | TOKEN_INCLUDE
    | TOKEN_DATA
    | TOKEN_NAMESPACE
    | TOKEN_IS
    | TOKEN_FUN
    | TOKEN_GT
    | TOKEN_GEQ
    | TOKEN_LT
    | TOKEN_LEQ
    | TOKEN_NEQ
    | TOKEN_MAIN
    | TOKEN_ARITHMETIC
    | TOKEN_MODULE
    | TOKEN_PRIORITY
    | TOKEN_LEMIT
    | TOKEN_REMIT
    | TOKEN_COMMA
    | TOKEN_LBRACKET
    | TOKEN_RBRACKET
    | TOKEN_DOT
    | TOKEN_APOSTROPHE
    | TOKEN_HASH
    | TOKEN_COLON
    | TOKEN_NEWLINE
    | TOKEN_BIND
    | TOKEN_LBRACE
    | TOKEN_RBRACE
    | TOKEN_LSQUARE
    | TOKEN_RSQUARE
    | TOKEN_BAR
    | TOKEN_ARROW
    | TOKEN_DARROW
    | TOKEN_EQUAL
    | TOKEN_EMIT
    | TOKEN_UNIT
    | TOKEN_STRING
    | TOKEN_ID
    | TOKEN_FLOAT
    | TOKEN_INT
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_newLineSeq
    | NONTERM_program
    | NONTERM_dottedPath
    | NONTERM_typeArg
    | NONTERM_commaPath
    | NONTERM_genericSeq
    | NONTERM_includeOrOpenStmts
    | NONTERM_includeOrOpenStmt
    | NONTERM_declarations
    | NONTERM_priority
    | NONTERM_declaration
    | NONTERM_typeOrNameDeclarations
    | NONTERM_typeOrNameDeclaration
    | NONTERM_typeVarsSeq
    | NONTERM_genericTypeDef
    | NONTERM_typeDeclaration
    | NONTERM_lambdaTypeDeclaration
    | NONTERM_arg
    | NONTERM_argSeq
    | NONTERM_literal
    | NONTERM_emit
    | NONTERM_functionCall
    | NONTERM_arithmeticCall
    | NONTERM_arithmeticExpr
    | NONTERM_comparisonOp
    | NONTERM_premises
    | NONTERM_conclusion
    | NONTERM_subtype
    | NONTERM_subtypes
    | NONTERM_mainOpt
    | NONTERM_rule
    | NONTERM_rules
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (ParserAST.Program) 
