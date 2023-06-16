namespace ChemicalEngine

module FakeClockSimulator =
    open Reaction

    let clockPhaseDuration = 20.0
    let private updateClockSpecies (clockPhases:int) time name =
        let currentPhase = (int (time / clockPhaseDuration)) % clockPhases
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


    let private addOffPeriodClockSpecies clockPhases crn :CRN =
        let offPeriodSpecies = 
            Seq.initInfinite id
                |> Seq.skip 1
                |> Seq.take clockPhases
                |> Seq.filter (fun i -> not <| Map.containsKey (Clock.clockSpeciesName i) (snd crn))
                |> Seq.map (Clock.clockSpecies clockPhases)
                |> Seq.toList

        let solution' = List.fold (fun m (n,c) -> Map.add n (n,c) m) (snd crn) offPeriodSpecies
        (fst crn, solution')

    let private appendClockSpecies clockPhases stepIndex (step:Step) =
        let activePhase = Clock.stepPeriod * stepIndex
        let species = Clock.clockSpecies clockPhases activePhase
        let addSpeciesToComponents components species = 
            (name species, 1) :: components

        let augmentCRN (crn:CRN) = 
            let reactions = fst crn |> List.map (fun (reactants, rate, products) -> 
                    (addSpeciesToComponents reactants species, rate, addSpeciesToComponents products species)) 
            
            let solution = Map.add (name species) species (snd crn)
            (reactions, solution)

        let addCmpPhase2 (crn:CRN) :CRN =
            let phase2ClockSpecies = Clock.clockSpecies clockPhases (activePhase+1)
            let (reactions, species) = Modules.cmpPhase2()
            let mapToComponents names :ReactionComponent list = 
                List.map (fun n -> (n,1)) names

            let mapNamedReaction names = 
                addSpeciesToComponents (mapToComponents names) phase2ClockSpecies

            let reactions' = List.map (fun (reactants, rate, products) -> 
                    mapNamedReaction reactants, rate, mapNamedReaction products) reactions

            let solution' = List.fold (fun m s -> Map.add (name s) s m) (snd crn) species
            (fst crn @ reactions', solution')

        match step with 
        | Normal (crn) -> augmentCRN crn
        | Cmp (crn) -> augmentCRN crn |> addCmpPhase2

    let private mergeSteps (crns:CRN list) :CRN=
        let reactions = List.collect fst crns
        let mapUnion m1 m2 = Map.fold (fun m k v -> Map.add k v m) m1 m2
        let solution = List.fold mapUnion Map.empty (List.map snd crns)
        (reactions, solution)

    let private ofFormula clockPhases (formula:Formula) =
        List.mapi (appendClockSpecies clockPhases) formula  |> mergeSteps |> addOffPeriodClockSpecies clockPhases


    let private watchWithClock clockPhases resF timeF duration crn =
        let names = snd crn |> Map.keys |> Set.ofSeq
        extractConcentrations names timeF duration (simulate clockPhases resF timeF crn)

    let watch resF timeF duration (formula:Formula) = 
        let clockPhases = Clock.stepPeriod * (List.length formula)
        let combinedCrn = ofFormula clockPhases formula
        printfn "%A" (fst combinedCrn)
        watchWithClock clockPhases resF timeF duration combinedCrn

    let watchConstRes duration formula =
        watch Simulator.constRes Simulator.constResTime duration formula

    let filterClockSpecies data =
        List.filter (fst >> Clock.isClockSpecies >> not) data


    let filterNames names data =
        List.filter (fun (n, _) -> List.contains n names) data

    let filterSpecies species data =
        filterNames (List.map name species) data