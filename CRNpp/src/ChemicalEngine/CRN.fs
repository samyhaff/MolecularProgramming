// Author: Roar Nind Steffensen, 16/06/2023

namespace ChemicalEngine

module CRN =
    open Reaction
    let fromReactionAndSpecies (reactions:Reaction list) (chemicals:Species list) :CRN =
        let solution = List.map (fun c -> name c, c) chemicals |> Map.ofList
        (reactions, solution)

    let collect (crns:CRN list) =
        let reactions = List.collect fst crns
        let mapUnion m1 m2 = Map.fold (fun m k v -> Map.add k v m) m1 m2
        let solution = List.fold mapUnion Map.empty (List.map snd crns)
        (reactions, solution)
