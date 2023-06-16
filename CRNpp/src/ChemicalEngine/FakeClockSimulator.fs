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
        let names = snd crn |> Map.keys |> Set.ofSeq
        extractConcentrations names timeF duration (simulate clockPhases resF timeF crn)

    let watch resF timeF duration (crnSteps:CRN list) = 
        let clockPhases = Clock.stepPeriod * List.length crnSteps
        let combinedCrn = asStepsWithClockSpecies clockPhases crnSteps
        watchWithClock clockPhases resF timeF duration combinedCrn

    let watchConstRes duration crnSteps =
        watch Simulator.constRes Simulator.constResTime duration crnSteps

    let filterClockSpecies data =
        List.filter (fst >> Clock.isClockSpecies >> not) data