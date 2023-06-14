open System
open Parser

[<EntryPoint>]
let main argv =
   let programName = AppDomain.CurrentDomain.FriendlyName
   if argv.Length = 0 then
      printfn "Usage: %s <filename>" programName
      1
   else
      let code = IO.File.ReadAllText(argv.[1])
      test pcrn code
      0
