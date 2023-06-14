namespace ChemicalEngine

module Simulator =
    open Reaction

    let dS ((reactions,solution):CRN) (name:Name) = 
        let rateProduct solution (reactants:Reactants) = 
            reactants 
             |> List.map (fun n -> Map.find n solution)
             |> List.fold (fun acc (C(_,c,m)) -> acc * (c ** m)) 1.0 

        let netChange (name:Name) (solution: Solution) (reactants:Reactants) (products:Products) :float = 
            let change (name:Name) (solution:Solution) (cs:Name list) = 
                List.filter (fun n -> n = name) cs
                |> List.map (fun n -> Map.find n solution)
                |> List.map (fun (C(_,_,m)) -> m)
                |> List.sum 
                |> float

            let changeS = change name solution
            in (changeS products) - (changeS reactants)

        let summands (name:Name) (solution:Solution) ((reactants, rate, products):Reaction) = 
            rate * (netChange name solution reactants products) * (rateProduct solution reactants)

        reactions |> List.map (summands name solution) |> List.sum |> float

    let run (crn:CRN) = 
        let stable (current:Solution) (next:Solution) =
            let stableConcentrations (C(_,c1,_)) (C(_,c2,_)) =
                abs (c2-c1) < 0.1
            in Map.forall (fun name chemical -> stableConcentrations chemical (Map.find name next)) current

        let updateChemical crn (C(n,c,m)) = 
            let concentationChange = dS crn n
            C(n, c + concentationChange, m)

        let updateCRN (crn:CRN) :CRN = 
            let updatedSolution = Map.map(fun name chemical -> updateChemical crn chemical) (snd crn)
            (fst crn, updatedSolution)

        let rec update crn n maxUpdates = 
            let next = updateCRN crn
            // printfn "Next: %A" (snd next |> Map.values |> Seq.toList)
            if (n = maxUpdates || stable (snd crn) (snd next)) 
            then next
            else update next (n+1) maxUpdates
        in update crn 0 100
