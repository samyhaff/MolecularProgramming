// Author: Roar Nind Steffensen, 13/06/2023

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

    type Command = | Cmp of CRN | Normal of CRN | Nested of Command list
    type Step = Command list
    type Formula = Step list

    let name (n,_) :Name= n
    let conc ((_,c):Species) = c
    let mult ((_,m):ReactionComponent) = m
