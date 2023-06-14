open System
open Parser
open TypeChecker

[<EntryPoint>]
let main argv =
   let programName = AppDomain.CurrentDomain.FriendlyName
   if argv.Length = 0 then
      printfn "Usage: %s <filename>" programName
      1
   else
      let code = IO.File.ReadAllText(argv.[1])
      let program = parse code
      printfn "%A" program
      printfn "%A" (checkProgram program)
      0
