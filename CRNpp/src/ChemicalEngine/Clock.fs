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
        if step = 0 || step = clockPhases-1 then 1.0 else 0.0
    
    let clockSpecies clockPhases step = 
        (clockSpeciesName step, clockSpeciescConc clockPhases step)
