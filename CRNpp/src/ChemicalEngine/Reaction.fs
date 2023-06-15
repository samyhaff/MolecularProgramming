namespace ChemicalEngine

module Reaction =

    type Name = string
    type Multiplicity = int
    type Concentration = float
    type Rate = float
    type Species = Name * Concentration
    type ReactionComponent = Name * Multiplicity
    type Reactants = ReactionComponent list
    type Products = ReactionComponent list
    type Reaction = Reactants * Rate * Products
    type Solution = Map<Name, Species>
    type CRN = Reaction list * Solution

    let name (n,_) :Name= n
    let conc ((_,c):Species) = c
    let mult ((_,m):ReactionComponent) = m


    let toCRN (reactions:Reaction list) (chemicals:Species list) :CRN =
        let solution = List.map (fun c -> name c, c) chemicals |> Map.ofList
        (reactions, solution)
