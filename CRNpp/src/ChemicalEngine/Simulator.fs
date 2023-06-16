// Author: Roar Nind Steffensen, 12/06/2023

namespace ChemicalEngine

module Simulator =
    open Reaction

    let private defaultRes = 0.01
    let stepsInDuration duration = duration / defaultRes |> ceil |> int
    let constResTime n = (float n) * defaultRes
    let constRes n = defaultRes

    let expRes n = 2.0 ** (-(float n)/8.0)

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

        let result = reactions |> Seq.map (summands name solution) |> Seq.sum |> float  |> (fun x -> x * resolution)
        // printfn "dS: %s: %f" name result
        result

    let update crn res =
        let updateChemical (name, conc) =
            let updatedConcentration = max 0.0 (conc + dS crn name res)
            (name, updatedConcentration)

        let updatedSolution = Map.map (fun _ chemical -> updateChemical chemical) (snd crn)
        in (fst crn, updatedSolution)
        
    let simulate resF (crn:CRN) :CRN seq = 
        let rec simulate' resF crn n =
            seq {yield crn; yield! simulate' resF (update crn (resF n)) (n+1)}
        in simulate' resF crn 1

    let extractConcentrations (names:Name Set) (steps:int) (crn:CRN seq) =
        let slns = Seq.take steps crn |> Seq.map snd |> Seq.toList
        List.map (fun n -> 
            (n, slns |> List.map (fun s -> Map.find n s |> conc))
        ) (Set.toList names)

    let rec watch resF (steps:int) (crn:CRN) = 
        let names = fst crn |> List.collect (fun (r,_,p) -> r @ p) |> List.map fst |> Set.ofList
        extractConcentrations names steps (simulate resF crn)

    let runToStable (resolution:float) (tolerance:float) (crn:CRN) = 
        let stable (current:Solution) (next:Solution) (tolerance:float) =
            let stableConcentrations (_,c1) (_,c2) =
                abs (c2-c1) < tolerance
            in Map.forall (fun name chemical -> stableConcentrations chemical (Map.find name next)) current

        let rec runner crn resolution tolerance n maxUpdates = 
            let next = update crn resolution
            if (stable (snd crn) (snd next) tolerance) 
            then next
            else if n = maxUpdates 
            then printfn "Max updates reached"
                 next
            else runner next resolution tolerance (n+1) maxUpdates
        in runner crn resolution tolerance 0 ((20.0/resolution) |> ceil |> int)
