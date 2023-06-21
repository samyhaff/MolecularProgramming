// Author: Roar Nind Steffensen, 14/06/2023

namespace Examples

module Modules =

    open ChemicalEngine
    open Reaction
    open Rendering.Plotting

    let private cmdToFormula cmd :Formula = 
        [[cmd]]
    let private watchCmd duration = 
        cmdToFormula >> Simulator.watch duration
    let private watchCmdCycle = 
        watchCmd Simulator.approxCycleDuration
    let private watchCmdCycleFiltered filter =
        cmdToFormula >> Simulator.watchFiltered Simulator.approxCycleDuration filter
    let private watchCmdCycleNoClock = 
        watchCmdCycleFiltered Simulator.removeClock

    let private showScatter title (xs,data)= 
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> showLabelledPlots title "time" "concentrations" (600,600)

    let watchModule moduleF label =
        moduleF () |> watchCmdCycle |> showScatter label

    let subAgtB () = 
        let A = ("A", 10.0)
        let B = ("B", 8.0)
        let C = ("C", 0.0)

        let filter = Simulator.onlyBySpecies [A;B;C]
        Modules.sub A B C |> watchCmdCycleFiltered filter |> showScatter "sub with A > B" 

    let subAltB () = 
        let A = ("A", 2.0)
        let B = ("B", 8.0)
        let C = ("C", 0.0)

        let filter = Simulator.onlyBySpecies [A;B;C]
        Modules.sub A B C |> watchCmdCycleFiltered filter |> showScatter "sub with A < B" 

    let add () = 
        let A = ("A", 2.0)
        let B = ("B", 8.0)
        let C = ("C", 0.0)

        Modules.add A B C |> watchCmdCycleNoClock |> showScatter "add" 

    let mul () = 
        let A = ("A", 2.0)
        let B = ("B", 8.0)
        let C = ("C", 0.0)

        Modules.mul A B C |> watchCmdCycleNoClock |> showScatter "mul" 

    let div () = 
        let A = ("A", 8.0)
        let B = ("B", 2.0)
        let C = ("C", 0.0)

        Modules.div A B C |> watchCmdCycleNoClock |> showScatter "div" 

    let divBy0 () = 
        let A = ("A", 8.0)
        let B = ("B", 0.0)
        let C = ("C", 0.0)

        Modules.div A B C |> watchCmdCycleNoClock |> showScatter "div by 0" 


    let sqrt () =
        let A = ("A", 25.0)
        let B = ("B", 0.0)
        Modules.sqrt A B |> watchCmdCycleNoClock |> showScatter "sqrt" 

    let clock () =
        let duration = 120.0
        let formula = [1..2] |> List.map (fun _ -> []) // empty steps to just see clock phases

        Simulator.watch duration formula |> showScatter "clock"

    let cmp () =
        let A = ("A", 2.0)
        let B = ("B", 5.0)

        Modules.cmp A B |> watchCmdCycleNoClock |> showScatter "cmp" 

    let ifGt () =
        let A = ("A", 5.0)
        let B = ("B", 8.0)
        let C = ("C", 0.0)

        let formula = [
            [Modules.cmp A B];
            [
                Modules.ifGt [Modules.ld A C]; 
                Modules.ifLt [Modules.ld B C]
            ]
        ]

        let duration = 90.0
        Simulator.watch duration formula |> showScatter "ifGt A > B -> C := A; A < B -> C := B"

