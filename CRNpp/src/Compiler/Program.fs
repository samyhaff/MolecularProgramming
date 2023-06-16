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

      let program = parse code
      let n = 10
      let xs = [1..n] |> List.map float
      BasicInterperter.run program 
         |> BasicInterperter.envSeqToConc n
         |> List.map (fun (n, ys) -> Rendering.Plotting.scatter xs ys n) 
         |> Rendering.Plotting.show "GCD basic interpreter"
      0
