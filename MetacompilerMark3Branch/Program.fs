open ParserAST 
open ParserUtils
open TypeChecker
open TypeCheckerTest
open System.IO
open Common
//open CodeGenerationCsharp
open CodeGenerationCSharp2
open Microsoft.FSharp.Text.Lexing

//let testParser (fileName : string) =
//  let inputChannel = new StreamReader(fileName)
//  let lexbuf = LexBuffer<char>.FromTextReader inputChannel
//  let parsedAST = Parser.start Lexer.tokenstream lexbuf
//  parsedAST


let parseFile (fileName : string) =
  let inputChannel = new StreamReader(fileName)
  let lexbuf = LexBuffer<char>.FromTextReader inputChannel
  let parsedAST = Parser.start Lexer.tokenstream lexbuf
  let processedAST = insertNamespaceAndFileName parsedAST fileName
  processedAST

[<EntryPoint>]
let main argv =
//  try
    let fileName = @"Content\Test\genericTest.mc"
    let outputFileName = @"Content\Test\codeGeneration.cs"
    let parsedAST = parseFile fileName
    let typedTest = checkProgram parsedAST
//    let codeGenerationTest = emitProgram typedTest
//    printfn "Type checking successful!"
//    File.WriteAllText(outputFileName, sprintf "%s" codeGenerationTest.Code)
    0
//  with
//  | TypeError(msg) -> 
//      printfn "%s" msg
//      1
//  | ParseError(msg, row, col) ->
//      printfn "%s at row %d column %d" msg row col
//      1
