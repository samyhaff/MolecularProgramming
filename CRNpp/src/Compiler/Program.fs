open System
open Parser
open TypeChecker
open Visualizer
open Interpreter

[<EntryPoint>]
let main argv =
   let programName = AppDomain.CurrentDomain.FriendlyName
   if argv.Length = 0 then
      printfn "Usage: %s <filename>" programName
      printfn "Running checks..."
      ChemicalEngine.Checks.runAll ()
      // Examples.Modules.watchModule (fun () -> ChemicalEngine.Modules.sub ("A",131.218572) ("B",24.45533649) ("C",46.70537003)) "sub test"
      // Examples.Modules.add ()
      // Examples.Modules.mul ()
      // Examples.Modules.div ()
      // Examples.Modules.clock 3
      // Examples.Modules.cmp ()
      // Examples.Modules.ifGt ()
      // Examples.Modules.clock 3
      Examples.Reactions.exampleReaction @"A: 1.0, B: 2.0, C: 0.0; A -> A + C, B -> B + C, C -> 0"
      // Examples.Formulas.factorial 5
      // Examples.Formulas.eulersConstant ()
      // Examples.Formulas.pi ()
      // Errors.Modules.add ()
      // Errors.Modules.sub ()
      // Errors.Modules.subAGtB ()
      // Errors.Modules.mul ()
      // Errors.Modules.div ()
   else
      let code = IO.File.ReadAllText(argv.[1])
      let program = parse code
      printf "%s" (ParserChecks.crnToString program)
      // printfn "%A" program
      // printfn "Initial Concentrations: %A" (getInitialConcentrations program)
      // printfn "%A" (convertAstToFormula program)
      run program 450.0 ["a"; "b"] "Test"
   0
