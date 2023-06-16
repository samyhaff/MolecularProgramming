namespace ChemicalEngine

module Clock =
    let stepPeriod = 3

    let isClockSpecies (name:string) =
        name.StartsWith("clc")

    let clockSpeciesName step = 
        $"clc{step}"
    let clockSpeciescConc clockPhases step = 
        if step = 0 || step = clockPhases-1 then 1.0 else 0.0
    
    let clockSpecies clockPhases step = 
        (clockSpeciesName step, clockSpeciescConc clockPhases step)
