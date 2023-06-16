namespace ChemicalEngine

module FakeClockSimulator =
    open Reaction

    let clockPhaseDuration = 20.0
    let private updateClockSpecies (clockPhases:int) time name =
        let currentPhase = 1 + (int (time / clockPhaseDuration)) % clockPhases
        let conc' = if name = $"clc{currentPhase}" then 2.0 else 0.0
        conc'

    let private update (clockPhases:int) (crn:CRN) res time =
        let updateChemical ((name, conc):Species) =
            let updatedConcentration = 
                if (name.StartsWith "clc") 
                then updateClockSpecies clockPhases time name
                else max 0.0 (conc + Simulator.dS crn name res)
            (name, updatedConcentration)

        let updatedSolution = Map.map (fun _ chemical -> updateChemical chemical) (snd crn)
        in (fst crn, updatedSolution)
        
    let private addClock phases (crn:CRN) :CRN= 
        let clock = Modules.clock phases
        (fst crn @ fst clock, Map.fold (fun m k v -> Map.add k v m) (snd crn) (snd clock))
        
    let private simulate clockPhases resF timeF (crn:CRN) :CRN seq = 
        let rec simulate' (crn:CRN) n =
            let next = update clockPhases crn (resF n) (timeF n)
            seq {yield crn; yield! simulate' next (n+1)}
        in simulate' crn 1

    let private extractConcentrations (names:Name Set) timeF (duration:float) (crn:CRN seq) =
        let slns = Seq.initInfinite timeF 
                    |> Seq.zip crn 
                    |> Seq.takeWhile (fun (c,t) -> t < duration) 
                    |> Seq.map (fun ((_,s),_) -> s)
                    |> Seq.toList

        List.map (fun n -> 
            (n, slns |> List.map (fun s -> Map.find n s |> conc))
        ) (Set.toList names)

    let private watchWithClock clockPhases resF timeF duration crn =
        let names = fst crn |> List.collect (fun (r,_,p) -> r @ p) |> List.map fst |> Set.ofList
        extractConcentrations names timeF duration (simulate clockPhases resF timeF crn)

    let watch clockPhases resF timeF duration (crn:CRN) = 
        watchWithClock clockPhases resF timeF duration (addClock clockPhases crn)

    let watchConstRes clockPhases duration crn =
        watch clockPhases Simulator.constRes Simulator.constResTime duration crn
