namespace ChemicalEngine

module Simulator =
    open Reaction

    let dS ((reactions,solution):CRN) (name:Name) = 
        let rateProduct solution (reactants:Reactants) = 
            reactants 
             |> List.map (fun n -> Map.find n solution)
             |> List.fold (fun acc (S(_,c,m)) -> acc * (c ** m)) 1.0 

        let netChange (name:Name) (solution: Solution) (reactants:Reactants) (products:Products) :float = 
            let change (name:Name) (solution:Solution) (cs:Name list) = 
                List.filter (fun n -> n = name) cs
                |> List.map (fun n -> Map.find n solution)
                |> List.map mult
                |> List.sum 
                |> float

            let changeS = change name solution
            in (changeS products) - (changeS reactants)

        let summands (name:Name) (solution:Solution) ((reactants, rate, products):Reaction) = 
            rate * (netChange name solution reactants products) * (rateProduct solution reactants)

        reactions |> List.map (summands name solution) |> List.sum |> float

    let update crn =
        let updateChemical crn (S(n,c,m)) =
            let updatedConcentration = max 0.0 (c + dS crn n)
            S(n, updatedConcentration, m)

        let updatedSolution = Map.map(fun _ chemical -> updateChemical crn chemical) (snd crn)
        in (fst crn, updatedSolution)

    let runToStable (tolerance:float) (crn:CRN) = 
        let stable (current:Solution) (next:Solution) (tolerance:float) =
            let stableConcentrations (S(_,c1,_)) (S(_,c2,_)) =
                abs (c2-c1) < tolerance
            in Map.forall (fun name chemical -> stableConcentrations chemical (Map.find name next)) current

        let rec runner crn tolerance n maxUpdates = 
            let next = update crn
            // printfn "Next: %A" (snd next |> Map.values |> Seq.toList)
            if (n = maxUpdates || stable (snd crn) (snd next) tolerance) 
            then next
            else runner next tolerance (n+1) maxUpdates
        in runner crn tolerance 0 100
