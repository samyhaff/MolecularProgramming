open System
open Parser
open ChemicalEngine

[<EntryPoint>]
let main argv =
   let programName = AppDomain.CurrentDomain.FriendlyName
   if argv.Length = 0 then
      // Checks.runAll ()
      // let a,b,c = ("A", 72.23127391), ("B", 71.8824643), ("C", 24.83549962)
      // ModuleExamples.watchModule (fun () -> Modules.sub a b c) "sub test"
      
      ModuleExamples.clock 8
      

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
