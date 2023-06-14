namespace ChemicalEngine

module Simulator =
    open Reaction

    let dS ((reactions,solution):CRN) (name:Name) = 
        let rateProduct solution (reactants:Reactants) = 
            reactants 
             |> Seq.map (fun n -> Map.find n solution)
             |> Seq.fold (fun acc (S(_,c,m)) -> acc * pown c m) 1.0 

        let netChange (name:Name) (solution: Solution) (reactants:Reactants) (products:Products) :float = 
            let change (name:Name) (solution:Solution) (cs:Name list) = 
                Seq.filter (fun n -> n = name) cs
                |> Seq.map (fun n -> Map.find n solution)
                |> Seq.map mult
                |> Seq.sum 
                |> float

            let changeS = change name solution
            in (changeS products) - (changeS reactants)

        let summands (name:Name) (solution:Solution) ((reactants, rate, products):Reaction) = 
            let summand = rate * (netChange name solution reactants products) * (rateProduct solution reactants)
            summand

        reactions |> Seq.map (summands name solution) |> Seq.sum |> float  |> (fun x -> x * 0.05)

    let update crn =
        let updateChemical crn (S(n,c,m)) =
            let updatedConcentration = max 0.0 (c + dS crn n)
            S(n, updatedConcentration, m)

        let updatedSolution = Map.map(fun _ chemical -> updateChemical crn chemical) (snd crn)
        in (fst crn, updatedSolution)

        

    let extractConcentrations (names:Name Set) (steps:int) (crn:CRN seq) =
        let slns = Seq.take steps crn |> Seq.map snd |> Seq.toList
        List.map (fun n -> 
            (n, slns |> List.map (fun s -> Map.find n s |> conc))
        ) (Set.toList names)

    let rec watch (steps:int) (crn:CRN) = 
        let names = fst crn |> List.collect (fun (r,_,p) -> r @ p) |> Set.ofList

        let rec simulate crn = 
            seq {yield crn; yield! simulate (update crn)}

        extractConcentrations names steps (simulate crn)

    let runToStable (tolerance:float) (crn:CRN) = 
        let stable (current:Solution) (next:Solution) (tolerance:float) =
            let stableConcentrations (S(_,c1,_)) (S(_,c2,_)) =
                abs (c2-c1) < tolerance
            in Map.forall (fun name chemical -> stableConcentrations chemical (Map.find name next)) current

        let rec runner crn tolerance n maxUpdates = 
            let next = update crn
            if (stable (snd crn) (snd next) tolerance) 
            then next
            else if n = maxUpdates 
            then printfn "Max updates reached"
                 next
            else runner next tolerance (n+1) maxUpdates
        in runner crn tolerance 0 1_000
