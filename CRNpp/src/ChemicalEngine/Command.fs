// Author: Roar Nind Steffensen 16/06/2023

namespace ChemicalEngine

module Command =
    open Reaction
    let rec containsCmp = function
        | Cmp _ -> true
        | Nested (cmds) -> List.exists containsCmp cmds
        | _ -> false

    let rec toCRN = function
        | Normal (crn) -> crn
        | Cmp (crn) -> crn
        | Nested cmds -> List.map toCRN cmds |> CRN.collect