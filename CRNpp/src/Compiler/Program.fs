open System
open Rendering.Plotting
open System.Diagnostics

[<EntryPoint>]
let main argv =
   if argv.Length = 0 then
      let programName = AppDomain.CurrentDomain.FriendlyName
      printfn "Welcome to the CRN++ simulator"
      printfn "Invoke the simulator in one of 3 ways:"
      printfn "dotnet run checks - runs implemented property checks"
      printfn "dotnet run example <example-name> - simulates the predefined example"
      printfn "dotnet run <crn-program> <?focused species eg: a b c> - simulates crn program with optional species filtering"
   else if argv.Length = 1 && argv[0] = "checks" then
      printfn "Running checks..."
      ChemicalEngine.Checks.runAll ()
      Checks.ParserChecks.runAll ()
   else if argv.Length = 2 && argv[0] = "example" then
      // printfn "Usage: %s <filename> <?focused species eg: a b c>" programName
      match argv[1] with
      | "module:add" -> Examples.Modules.add ()
      | "module:subA>B" -> Examples.Modules.subAgtB ()
      | "module:subA<B" -> Examples.Modules.subAltB ()
      | "module:mul" -> Examples.Modules.mul ()
      | "module:div" -> Examples.Modules.div ()
      | "module:divBy0" -> Examples.Modules.divBy0 ()
      | "module:sqrt" -> Examples.Modules.sqrt ()
      | "module:clock" -> Examples.Modules.clock ()
      | "module:cmp" -> Examples.Modules.cmp ()
      | "module:ifGt" -> Examples.Modules.ifGt ()
      | "module:rxn" -> Examples.Modules.rxn ()
      | "reaction" -> Examples.Reactions.exampleReaction @"A: 1.0, B: 2.0, C: 0.0; A -> A + C, B -> B + C, C -> 0"
      | "formula:fact5" -> Examples.Formulas.factorial 5
      | "formula:e" -> Examples.Formulas.eulersConstant ()
      | "formula:pi" -> Examples.Formulas.pi ()
      | "formula:discrete_counter" -> Examples.Formulas.discrete_counter 3
      | "formula:division" -> Examples.Formulas.division 20 3
      | "formula:integer_square_root" -> Examples.Formulas.integer_square_root 10
      | "performance:bigCRN" -> Examples.Formulas.performanceTest ()
      | "errors:add" -> Errors.Modules.add ()
      | "errors:sub" -> Errors.Modules.sub ()
      | "errors:subA>B" -> Errors.Modules.subAGtB ()
      | "errors:subClose" -> Errors.Modules.subOneOff ()
      | "errors:subOne" -> Errors.Modules.subMinus1 ()
      | "errors:mul" -> Errors.Modules.mul ()
      | "errors:div" -> Errors.Modules.div ()
      | _ -> failwith "Unknown example"
   else
      let start = Stopwatch.GetTimestamp()
      let programFileName = argv[0]
      let title = programFileName
      let duration = 1000.0

      printfn $"Loading crn program: {title}"
      let code = IO.File.ReadAllText(programFileName)
      let focusSpecies = argv |> Seq.skip 1 |> Seq.toList

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
