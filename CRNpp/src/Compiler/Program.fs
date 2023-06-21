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
      Checks.ParserChecks.runAll ()
      // Examples.Modules.watchModule (fun () -> ChemicalEngine.Modules.sub ("A",131.218572) ("B",24.45533649) ("C",46.70537003)) "sub test"
      // Examples.Modules.add ()
      // Examples.Modules.subAgtB ()
      // Examples.Modules.subAltB ()
      // Examples.Modules.mul ()
      // Examples.Modules.div ()
      // Examples.Modules.divBy0 ()
      // Examples.Modules.sqrt ()
      // Examples.Modules.clock ()
      // Examples.Modules.cmp ()
      // Examples.Modules.ifGt ()
      // Examples.Modules.clock 3
      // Examples.Reactions.exampleReaction @"A: 1.0, B: 2.0, C: 0.0; A -> A + C, B -> B + C, C -> 0"
      // Examples.Formulas.factorial 5
      // Examples.Formulas.eulersConstant ()
      // Examples.Formulas.pi ()
      // Examples.Formulas.discrete_counter 3
      // Examples.Formulas.division 20 3
      // Examples.Formulas.integer_square_root 10
      // Errors.Modules.add ()
      // Errors.Modules.sub ()
      // Errors.Modules.subAGtB ()
      // Errors.Modules.subOneOff ()
      // Errors.Modules.subMinus1 ()
      // Errors.Modules.mul ()
      // Errors.Modules.div ()
   else
      let code = IO.File.ReadAllText(argv.[1])
      let program = parse code
      // printf "%s" (ParserChecks.crnToString program)
      // printfn "%A" program
      // printfn "Initial Concentrations: %A" (getInitialConcentrations program)
      // printfn "%A" (convertAstToFormula program)
      // printfn "%A" (ParserChecks.parserCheck program)
      run program 600.0 ["a"; "b"] "Test"
   0
