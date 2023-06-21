module InterpreterChecks

open FsCheck
open Ast

let basicInterpreterRun (program: Crn)  (steps: int) =
    BasicInterperter.run program
    |> BasicInterperter.envSeqToConc steps

let crnInterpreterRun (program: Crn) (steps: int) =
   let formula = Interpreter.convertAstToFormula program
   formula |> ChemicalEngine.Simulator.watch (20.0 * float steps)
