namespace Examples

module Reactions =
    open ChemicalEngine
    open Reaction
    open Rendering.Plotting

    let private cmdToFormula cmd :Formula = [[cmd]]
    let private watchCmd duration = cmdToFormula >> Simulator.watch duration
    let private watchCmdCycle = watchCmd Simulator.approxCycleDuration

    let exampleReaction input =
        let species, reactions = Parser.parse input
        let (xs, data) = CRN.fromReactionAndSpecies reactions species |> Normal |> watchCmdCycle
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "Example Reaction"
