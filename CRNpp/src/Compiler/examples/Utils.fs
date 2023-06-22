namespace Examples

module Utils = 
    open ChemicalEngine
    open Rendering.Plotting
    open System.Diagnostics

    let plot title (xs,data) = 
        data |> List.map (fun (n, ys) -> line xs ys n) 
             |> showLabelledPlots title "time" "concentrations" (600, 800)

    let showDuration duration filter title formula = 
        printfn "Total steps: %d" (List.length formula * Clock.stepPeriod)
        let start = Stopwatch.GetTimestamp () 
        let (xs,data) = 
            formula 
            |> Simulator.watch duration
            |> Simulator.shrinkData
        
        printfn "Total #species: %d" (List.length data)
        printfn "Total simulation time: %.2f s" (Stopwatch.GetElapsedTime start).TotalSeconds
        (xs, filter data) |> plot title

    let showSpecies duration species title formula =
        let filter = Simulator.onlyBySpecies species
        showDuration duration filter title formula
    
    let showAll duration title formula = 
        showDuration duration id title formula
        