open ParserAST
open TypeChecker
open TypeCheckerTest

[<EntryPoint>]
let main argv =
  let typedTest1 = checkProgram tcTest
  printfn "%A" argv
  0
