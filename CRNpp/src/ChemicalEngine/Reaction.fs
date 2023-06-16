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

    let private addOffPeriodClockSpecies clockPhases crn :CRN =
        let offPeriodSpecies = 
            Seq.initInfinite id
                |> Seq.skip 1
                |> Seq.take clockPhases
                |> Seq.filter (fun i -> i % Clock.stepPeriod <> 0) 
                |> Seq.map (Clock.clockSpecies clockPhases)
                |> Seq.toList

        let solution' = List.fold (fun m (n,c) -> Map.add n (n,c) m) (snd crn) offPeriodSpecies
        (fst crn, solution')

    let private appendClockSpecies clockPhases step crn =
        let species = Clock.clockSpecies clockPhases (Clock.stepPeriod * (step+1))
        let addSpeciesToComponents components = 
            (name species, 1) :: components

        let reactions = fst crn |> List.map (fun (reactants, rate, products) -> 
                (addSpeciesToComponents reactants, rate, addSpeciesToComponents products)) 
        
        let solution = Map.add (name species) species (snd crn)
        (reactions, solution)

    let private mergeSteps (crns:CRN list) :CRN=
        let reactions = List.collect fst crns
        let mapUnion m1 m2 = Map.fold (fun m k v -> Map.add k v m) m1 m2
        let solution = List.fold mapUnion Map.empty (List.map snd crns)
        (reactions, solution)

    let asStepsWithClockSpecies clockPhases (crns:CRN list) =
        List.mapi (appendClockSpecies clockPhases) crns  |> mergeSteps |> addOffPeriodClockSpecies clockPhases


