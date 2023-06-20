// Author: Roar Nind Steffensen, 16/06/2023

namespace Examples

module Formulas = 
    open ChemicalEngine
    open ChemicalEngine.Modules
    open Rendering.Plotting

    let showCycles duration species title formula = 
        let filter = Simulator.onlyBySpecies species
        let plot (xs,data) = 
            data |> List.map (fun (n, ys) -> line xs ys n) |> showPlots title

        Simulator.watch duration formula |> plot
        Simulator.watchFiltered duration filter formula |> plot

        // let (xs, data) = Simulator.watchFiltered duration filter formula |> Simulator.shrinkData
        // data |> List.map (fun (n, ys) -> line xs ys n) |> showPlots title
        
    let factorial n = 
    
        let fS = ("f", 1.0)
        let oneS = ("one", 1.0)
        let iS = ("i", float n)
        let fnextS = ("fnext", 0.0)
        let inextS = ("inext", 0.0)

        let formula = [
            [
                cmp iS oneS; 
                mul fS iS fnextS;
                sub iS oneS inextS
            ];
            [
                ifGt [
                    ld inextS iS;
                    ld fnextS fS;
                ]
            ]
        ]

        showCycles 800.0 [fS] $"factorial: {n}!" formula

    let eulersConstant () =
        let eS = ("e", 1.0)
        let elementS = ("element", 1.0)
        let divisorS = ("divisor", 1.0)
        let oneS = ("one", 1.0)
        // let divisorMultiplierS = ("divisorMultiplier", 1.0) // included in article CRN but not used?

        let eNextS = ("eNext", 0.0)
        let elementNextS = ("elementNext", 0.0)
        let divisorNextS = ("divisorNext", 0.0)

        let formula = [
            [
                div elementS divisorS elementNextS;
                add divisorS oneS divisorNextS;
                add eS elementNextS eNextS;
            ];
            [
                ld elementNextS elementS;
                ld divisorNextS divisorS;
                ld eNextS eS;
            ]
        ]

        showCycles 500.0 [eS] "eulers constant 2.718..." formula


    let pi () = 
        let fourS = ("four", 4.0)
        let divisor1S = ("divisor1", 1.0)
        let divisor2S = ("divisor2", 3.0)
        let piS = ("pi", 0.0)

        let divisor1NextS = ("divisor1Next", 0.0)
        let divisor2NextS = ("divisor2Next", 0.0)
        let piNextS = ("piNext", 0.0)
        let factorS = ("factor", 0.0)
        let factor1S = ("factor1", 0.0)
        let factor2S = ("factor2", 0.0)

        let formula = [
            [
                div fourS divisor1S factor1S;
                add divisor1S fourS divisor1NextS;
                div fourS divisor2S factor2S;
                add divisor2S fourS divisor2NextS;
                sub factor1S factor2S factorS;
                add piS factorS piNextS;
            ];
            [
                ld divisor1NextS divisor1S
                ld divisor2NextS divisor2S
                ld piNextS piS
            ]
        ]
        showCycles 500.0 [piS] "pi 3.1415..." formula
