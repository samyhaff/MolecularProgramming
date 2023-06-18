// Author: Roar Nind Steffensen, 16/06/2023

namespace ChemicalEngine

module Clock =
    let stepPeriod = 2
    let stepPeriodF = float stepPeriod

    let isClockSpecies (name:string) =
        name.StartsWith("clc")

    let clockSpeciesName step = 
        $"clc{step}"
    let clockSpeciescConc clockPhases step = 
        let offset = 0.0000000089714 // configured for a 3 phase clock to have a phase duration of 20
        let mainConc = 0.5 - offset / 2.0
        let altConc = offset / (float clockPhases - 2.0)
        if step = 0 || step = clockPhases-1 then mainConc else altConc
    
    let clockSpecies clockPhases step = 
        (clockSpeciesName step, clockSpeciescConc clockPhases step)
