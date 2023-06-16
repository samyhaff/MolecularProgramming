namespace ChemicalEngine

module Clock =
    let stepPeriod = 3

    let isClockSpecies (name:string) =
        name.StartsWith("clc")

    let clockSpeciesName step = 
        $"clc{step}"
    let clockSpeciescConc clockPhases step = 
        if step = 1 || step = clockPhases then 2.0 else 0.0
    
    let clockSpecies clockPhases step = 
        (clockSpeciesName step, clockSpeciescConc clockPhases step)
