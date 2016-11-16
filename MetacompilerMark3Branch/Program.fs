open ParserAST 
open ParserUtils
open TypeChecker
open TypeCheckerTest
open System.IO
open Common
open CodeGenerationCsharp
open Microsoft.FSharp.Text.Lexing

let parseFile (fileName : string) =
  let inputChannel = new StreamReader(fileName)
  let lexbuf = LexBuffer<char>.FromTextReader inputChannel
  let parsedAST = Parser.start Lexer.tokenstream lexbuf
  let processedAST = insertNamespaceAndFileName parsedAST fileName
  processedAST

[<EntryPoint>]
let main argv =
  try
    let fileName = @"Content\Test\test3.mc"
    let outputFileName = @"Content\Test\codeGeneration.cs"
    let parsedAST = parseFile fileName
//    printfn "%s" (parsedAST.ToString())
//    let typedTest = checkProgram parsedAST
//    let codeGenerationTest = emitProgram typedTest
//    printfn "Type checking successful!"
//    File.WriteAllText("TypeCheckerOutput.txt",sprintf "%A" typedTest)
//    File.WriteAllText(outputFileName, sprintf "%s" codeGenerationTest)
    0
  with
  | TypeError(msg) -> 
      printfn "%s" msg
      1
  | ParseError(msg, row, col) ->
      printfn "%s at row %d column %d" msg row col
      1
