open ParserAST
open TypeChecker
open TypeCheckerTest
open System.IO

[<EntryPoint>]
let main argv =
  try
    let typedTest = checkProgram test1
    printfn "Type checking successful!"
    File.WriteAllText("TypeCheckerOutput.txt",sprintf "%A" typedTest)
    0
  with
  | TypeError(msg) -> 
      printfn "%s" msg
      1
