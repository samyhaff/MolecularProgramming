// Author: Roar Nind Steffensen, 20/06/2023

namespace Examples

module Formulas = 
    open ChemicalEngine
    open Modules
    open Reaction
    open Rendering.Plotting
    open Utils
        
    let factorial n = 
        printfn "Calculating factorial formula"
    
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

        showSpecies 800.0 [fS] $"factorial: {n}!" formula

    let eulersConstant () =
        printfn "Calculating eulers constant formula"

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

        showSpecies 350.0 [eS] "eulers constant 2.718..." formula


    let pi () = 
        printfn "Calculating pi formula"

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
        showSpecies 400.0 [piS] "pi 3.1415..." formula


    let discrete_counter n =
        printfn $"discrete counter formula for {n}"

        let cS = ("c", float n)
        let cinitialS = ("cInitial", conc cS)
        let oneS = ("one", 1.0)
        let zeroS = ("zero", 0.0)

        let cnextS = ("cnext", 0.0)

        let formula = [
            [
                sub cS oneS cnextS;
                cmp cS zeroS;
            ]
            [
                ifGt [ ld cnextS cS]
                ifLe [ ld cinitialS cS]
            ]
        ]

        showSpecies 2000.0 [cS] $"discrete counter of {n}" formula

    let division a0 b0 =
        printfn $"division formula for a0={a0} and b0={b0}"

        let aS = ("a", float a0)
        let bS = ("b", float b0)
        let oneS = ("one", 1.0)

        let anextS = ("anext", 0.0)
        let qS = ("q", 0.0)
        let qnextS = ("qnext", 0.0)
        let rS = ("r", 0.0)

        let formula = [
            [ // step 1
                cmp aS bS;
            ]
            [ // step 2
                ifGe [
                    sub aS bS anextS
                    add qS oneS qnextS
                ]
            ]
            [ // step 3
                ifGe [
                    ld anextS aS
                    ld qnextS qS
                ]
                ifLt [ld aS rS]
            ]
        ]
        showSpecies 2000.0 [aS;bS;qS;rS;] $"division a0={a0}, b0={b0}" formula

    let integer_square_root n0 =
        printfn $"integer square root formula for n0={n0}"

        let nS = ("n", float n0)
        let oneS = ("one", 1.0)

        let zS = ("z", 0.0)
        let znextS = ("znext", 0.0)
        let zpowS = ("zpow", 0.0)
        let outS = ("out", 0.0)

        let formula = [
            [ // step 1
                add zS oneS znextS
                mul znextS znextS zpowS
                cmp zpowS nS
            ]
            [ // step 2
                ifLt [ld znextS zS]
                ifGe [ld zS outS]
            ]
        ]

        showSpecies 2000.0 [zS; zpowS; outS] $"integer square root n0={n0}" formula

    let simulatorPerformance () = 
        let nAddModules n =
            [0..n] 
                |> List.map (fun i -> add ($"a{i}", 1.0) ($"b{i}", 2.0) ($"c{i}", 0.0))

        let parallel_compilations n :CRN list= 
            Seq.initInfinite id
            |> Seq.map (fun i -> nAddModules i |> List.map Command.toCRN |> CRN.collect)
            |> Seq.take n
            |> Seq.toList

        let step_compilations n :Formula list= 
            Seq.initInfinite id
            |> Seq.map (fun i -> [0..i] |> List.map (fun _ -> nAddModules 1))
            |> Seq.take n
            |> Seq.toList

        let steps = 100_000
        let testTime simulator i compilation =
            let start = System.Diagnostics.Stopwatch.GetTimestamp ()
            compilation |> simulator |> Seq.skip steps |> Seq.head |> ignore
            let elapsedMs = (System.Diagnostics.Stopwatch.GetElapsedTime start).TotalMilliseconds
            printfn $"{i+1}: {elapsedMs} ms"
            elapsedMs

        printfn "CRN's of n parallel add modules: Slow simulator"
        let slowData = parallel_compilations 4 |> List.mapi (testTime (Simulator.simulate' Simulator.slowCrnUpdater))

        printfn "CRN's of n parallel add modules: Fast simulator"
        let fastData = parallel_compilations 15 |> List.mapi (testTime Simulator.simulateFast)
        let data = [("fast", fastData); ("original", slowData)]

        data |> List.map (fun (n, ys) -> line ([1..List.length ys] |> List.map float) ys n) |> showPlots "performance improvement for n parallel add modules in 1 step"


        printfn "CRN's of n steps each with 1 add module: Slow simulator"
        let slowData = step_compilations 4 |> List.mapi (testTime ((Simulator.simulateFormula' Simulator.slowCrnUpdater)>>snd))

        printfn "CRN's of n steps each with 1 add module: Fast simulator"
        let fastData = step_compilations 15 |> List.mapi (testTime (Simulator.simulateFormula>>snd))
        let data = [("fast", fastData); ("original", slowData)]

        data |> List.map (fun (n, ys) -> line ([1..List.length ys] |> List.map float) ys n) |> showPlots "performance improvement for n steps each with 1 add module"


    let performanceTest () =
        let A = ("A", 10.0)
        let B = ("B", 11.0)
        let C = ("C", 12.0)
        let one = ("one", 1.0)

        let Anext = ("Anext", 0.0)
        let Bnext = ("Bnext", 0.0)
        let Cnext = ("Cnext", 0.0)

        let addStep = [
            add A one Anext
            add B one Bnext
            add C one Cnext
        ]
        let subStep = [
            sub A one Anext
            sub B one Bnext
            sub C one Cnext
        ]
        let loadStep = [
            ld Anext A
            ld Bnext B
            ld Cnext C
        ]

        let formula = 
            [1..4] 
            |> List.collect (fun _ -> [addStep; loadStep; subStep; loadStep])
        

        let steps = List.length formula 
        let duration = 5.0 * float steps * Clock.stepPeriodF * Simulator.approxCycleDuration
        // showAll duration "Big CRN test" formula
        showSpecies duration [A;B;C] "Big CRN test" formula
