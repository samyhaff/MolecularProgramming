// Author: Roar Nind Steffensen, 14/06/2023

namespace Examples

module Modules =

    open ChemicalEngine
    open Reaction
    open Rendering.Plotting
    open Utils

    let private cmdToFormula cmd :Formula = 
        [[cmd]]

    let private showCommand title cmd =
        cmd |> cmdToFormula |> showNoClock Simulator.approxCycleDuration title

    let subAgtB () = 
        let A = ("A", 10.0)
        let B = ("B", 8.0)
        let C = ("C", 0.0)
        Modules.sub A B C |> showCommand "sub with A > B" 

    let subAltB () = 
        let A = ("A", 2.0)
        let B = ("B", 8.0)
        let C = ("C", 0.0)
        Modules.sub A B C |> showCommand "sub A - B with A < B" 

    let add () = 
        let A = ("A", 2.0)
        let B = ("B", 8.0)
        let C = ("C", 0.0)
        Modules.add A B C |> showCommand "add A + B" 

    let mul () = 
        let A = ("A", 2.0)
        let B = ("B", 8.0)
        let C = ("C", 0.0)
        Modules.mul A B C |> showCommand "mul A * B" 

    let div () = 
        let A = ("A", 8.0)
        let B = ("B", 2.0)
        let C = ("C", 0.0)
        Modules.div A B C |> showCommand "div A / B" 

    let divBy0 () = 
        let A = ("A", 8.0)
        let B = ("B", 0.0)
        let C = ("C", 0.0)
        Modules.div A B C |> showCommand "div A / 0" 


    let sqrt () =
        let A = ("A", 25.0)
        let B = ("B", 0.0)
        Modules.sqrt A B |> showCommand "sqrt A" 

    let clock () =
        let duration = 120.0
        let formula = [1..2] |> List.map (fun _ -> []) // empty steps to just see clock phases
        showAll duration "clock" formula

    let cmp () =
        let A = ("A", 2.0)
        let B = ("B", 5.0)
        Modules.cmp A B |> showCommand "cmp A B" 

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
        formula |> showAll duration "ifGt A > B -> C := A; A < B -> C := B"


    let rxn () =
        let A = ("A", 2.0)
        let B = ("B", 4.0)

        let formula = [
            [
                Modules.rxn [A] 1.0 [A; B]
                Modules.rxn [B] 1.0 []
            ]
        ]

        let duration = 20.0
        formula |> showAll duration "Load A->B via rxn module"
