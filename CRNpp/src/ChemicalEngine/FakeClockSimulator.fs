// Author: Roar Nind Steffensen, 16/06/2023

namespace ChemicalEngine

module FakeClockSimulator =
    open Reaction

    let clockPhaseDuration = 20.0
    let private updateClockSpecies (clockPhases:int) time name =
        let currentPhase = (int (time / clockPhaseDuration)) % clockPhases
        let conc' = if name = $"clc{currentPhase}" then 2.0 else 0.0
        conc'

    let private update (clockPhases:int) time (crn:CRN) =
        let updateChemical ((name, conc):Species) =
            let updatedConcentration = 
                if (name.StartsWith "clc") 
                then updateClockSpecies clockPhases time name
                else max 0.0 (conc + Simulator.dS crn name)
            (name, updatedConcentration)

        let updatedSolution = Map.map (fun _ chemical -> updateChemical chemical) (snd crn)
        in (fst crn, updatedSolution)

    let simulateFormula (formula:Formula) =
        let clockPhases = Clock.stepPeriod * (List.length formula)
        Simulator.simulateFormula' (update clockPhases) formula

    let watch duration (formula:Formula) = 
        let (names, crns) = simulateFormula formula
        Simulator.extractConcentrations names duration crns

    let watchFiltered duration filter formula = 
        let (xs, data) = watch duration formula
        in (xs, filter data)