open System
open Rendering.Plotting
open System.Diagnostics

[<EntryPoint>]
let main argv =
   let programName = AppDomain.CurrentDomain.FriendlyName
   if argv.Length = 1 && argv[0] = "checks"
   then 
      printfn "Running checks..."
      ChemicalEngine.Checks.runAll ()
      Checks.ParserChecks.runAll ()
   else if argv.Length <= 1 then
      printfn "Usage: %s <filename> <?focused species eg: a b c>" programName
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
      let start = Stopwatch.GetTimestamp()
      let programFileName = argv[1]
      let title = programFileName
      let duration = 1000.0

      printfn $"Loading crn program: {title}"
      let code = IO.File.ReadAllText(programFileName)
      let focusSpecies = argv |> Seq.skip 2 |> Seq.toList

      printfn "Parsing crn program"
      let program = Parser.parse code
      
      printfn "Compiling crn compilation"
      let formula = Interpreter.convertAstToFormula program

      printfn $"Simulating crn for duration: {duration}"
      let filter = if List.isEmpty focusSpecies then id else ChemicalEngine.Simulator.onlyByNames focusSpecies
      let data = Interpreter.eval formula filter duration

      printfn "Plotting simulation"
      data |> linePlots |> showLabelledPlots title "time" "concentrations" (800, 800)
      printfn "Finished"
      printfn "Total time: %.2f s" (Stopwatch.GetElapsedTime start).TotalSeconds
   0
