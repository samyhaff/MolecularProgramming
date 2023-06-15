namespace ChemicalEngine

module Simulator =
    open Reaction

    let dS ((reactions,solution):CRN) (name:Name) (resolution: float)= 
        let rateProduct solution (reactants:Reactants) = 
            reactants 
             |> Seq.map (fun (n, m) -> (Map.find n solution, m))
             |> Seq.fold (fun acc ((_,c), m) -> acc * pown c m) 1.0 

        let netChange (name:Name) (reactants:Reactants) (products:Products) :float = 
            let change (name:Name) (cs:ReactionComponent list) = 
                Seq.filter (fun (n,_) -> n = name) cs
                |> Seq.map mult
                |> Seq.sum 
                |> float

            let changeS = change name
            in (changeS products) - (changeS reactants)

        let summands (name:Name) (solution:Solution) ((reactants, rate, products):Reaction) = 
            let summand = rate * (netChange name reactants products) * (rateProduct solution reactants)
            summand

        reactions |> Seq.map (summands name solution) |> Seq.sum |> float  |> (fun x -> x * resolution)

    let update crn resolution =
        let updateChemical crn (n,c) =
            let updatedConcentration = max 0.0 (c + dS crn n resolution)
            (n, updatedConcentration)

        let updatedSolution = Map.map(fun _ chemical -> updateChemical crn chemical) (snd crn)
        in (fst crn, updatedSolution)
        

    let extractConcentrations (names:Name Set) (steps:int) (crn:CRN seq) =
        let slns = Seq.take steps crn |> Seq.map snd |> Seq.toList
        List.map (fun n -> 
            (n, slns |> List.map (fun s -> Map.find n s |> conc))
        ) (Set.toList names)

    let rec watch (resolution:float) (steps:int) (crn:CRN) = 
        let names = fst crn |> List.collect (fun (r,_,p) -> r @ p) |> List.map fst |> Set.ofList

        let rec simulate crn = 
            seq {yield crn; yield! simulate (update crn resolution)}

        extractConcentrations names steps (simulate crn)

    let runToStable (resolution:float) (tolerance:float) (crn:CRN) = 
        let stable (current:Solution) (next:Solution) (tolerance:float) =
            let stableConcentrations (_,c1) (_,c2) =
                abs (c2-c1) < tolerance
            in Map.forall (fun name chemical -> stableConcentrations chemical (Map.find name next)) current

        let rec runner crn resolution tolerance n maxUpdates = 
            let next = update crn resolution
            // if (stable (snd crn) (snd next) tolerance) 
            // then next
            // else if n = maxUpdates 
            // then printfn "Max updates reached"
            if n = maxUpdates
            then next
            else runner next resolution tolerance (n+1) maxUpdates
        in runner crn resolution tolerance 0 ((20.0/resolution) |> ceil |> int)
