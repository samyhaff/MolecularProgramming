open System
open Parser
open TypeChecker

[<EntryPoint>]
let main argv =
   let programName = AppDomain.CurrentDomain.FriendlyName
   if argv.Length = 0 then
      printfn "Usage: %s <filename>" programName
      printfn "Running checks..."
      // ChemicalEngine.Checks.runAll ()
      // Examples.Modules.watchModule (fun () -> ChemicalEngine.Modules.sub ("A",131.218572) ("B",24.45533649) ("C",46.70537003)) "sub test"
      // Examples.Modules.cmp()
      // Examples.Clocks.fakeClock()
      // Examples.Modules.ifGt ()
      // Examples.Modules.clock 3
      Examples.Reactions.exampleReaction
      // Examples.Formulas.factorial 5
      // Examples.Formulas.eulersConstant ()
      // Examples.Formulas.pi ()
      0
   else
      // ChemicalEngine.Parser.test ChemicalEngine.Parser.preaction "A + A (0.5)-> C"

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
