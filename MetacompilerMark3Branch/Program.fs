open ParserAST 
open ParserUtils
open TypeChecker
open TypeCheckerTest
open System.IO
open Microsoft.FSharp.Text.Lexing


let parseFile (fileName : string) =
  let inputChannel = new StreamReader(fileName)
  let lexbuf = LexBuffer<char>.FromTextReader inputChannel
  let parsedAST = Parser.start Lexer.tokenstream lexbuf
  parsedAST

[<EntryPoint>]
let main argv =
  try
//    let testUtils = testRetAndArgs()
//    printfn "%s" (testUtils.ToString())
    let parsedAST = parseFile @"Content\Test\test1.mc"
//    printfn "%s" (parsedAST.ToString())
//    let typedTest = checkProgram test1
//    printfn "Type checking successful!"
//    File.WriteAllText("TypeCheckerOutput.txt",sprintf "%A" typedTest)
    0
  with
  | TypeError(msg) -> 
      printfn "%s" msg
      1
  | ParseError(msg, row, col) ->
      printfn "%s at row %d column %d" msg row col
      1
