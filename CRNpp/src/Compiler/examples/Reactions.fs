namespace Examples

module Reactions =
    open ChemicalEngine
    open Reaction
    open Rendering.Plotting

    let private n = 20.0
    let private watch = FakeClockSimulator.watchConstRes n
    let private cmdToFormula cmd :Formula = [[cmd]]
    let private watchCmd = cmdToFormula >> watch
    let private resF = Simulator.constRes
    let steps i = float i * resF i
    let private xs = Seq.initInfinite steps
                    |> Seq.takeWhile (fun i -> i < n)
                    |> Seq.toList

    let exampleReaction input =
        let species, reactions = Parser.parse input
        let data = CRN.fromReactionAndSpecies reactions species |> Normal |> watchCmd
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "sub with A > B"
