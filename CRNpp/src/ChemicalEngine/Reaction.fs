namespace ChemicalEngine

module Reaction =

    type Name = string
    type Multiplicity = int
    type Concentration = float
    type Rate = float
    type Chemical = S of Name * Concentration * Multiplicity
    type Reactants = Name list
    type Products = Name list
    type Reaction = Reactants * Rate * Products
    type Solution = Map<Name, Chemical>
    type CRN = Reaction list * Solution

    let name (S(n,_,_)) = n
    let conc (S(_,c,_)) = c
    let mult (S(_,_,m)) = m

    let toCRN (reactions:Reaction list) (chemicals:Chemical list) :CRN =
        let solution = List.zip (List.map name chemicals) chemicals |> Map.ofList
        (reactions, solution)