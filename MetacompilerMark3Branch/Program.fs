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
//    let id1 = { Namespace = "TestModule"; Name = "x";  }
//    let id2 = { Name = "y"; Namespace = "TestModule" }
//    let check = id1 <> id2
//    let m = Map.empty
//    let m = m.Add(id1,0)
//    let m = m.Add(id2,1) 
//    let idOpt = m |> Map.tryFind(id1)
//    printfn "%s" (testUtils.ToString())
    let fileName = @"Content\Test\test1.mc"
    let outputFileName = @"Content\Test\codeGeneration.cs"
    let parsedAST = parseFile fileName
//    printfn "%s" (parsedAST.ToString())
    let typedTest = checkProgram parsedAST
    let codeGenerationTest = emitProgram typedTest
    printfn "Type checking successful!"
    File.WriteAllText("TypeCheckerOutput.txt",sprintf "%A" typedTest)
    File.WriteAllText(outputFileName, sprintf "%s" codeGenerationTest)
    0
  with
  | TypeError(msg) -> 
      printfn "%s" msg
      1
  | ParseError(msg, row, col) ->
      printfn "%s at row %d column %d" msg row col
      1
