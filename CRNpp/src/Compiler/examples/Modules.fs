// Author: Roar Nind Steffensen, 14/06/2023

namespace Examples

module Modules =

    open ChemicalEngine
    open Reaction
    open Rendering.Plotting

    let private cmdToFormula cmd :Formula = [[cmd]]
    let private watchCmd duration = cmdToFormula >> Simulator.watch duration
    let private watchCmdCycle = watchCmd Simulator.approxCycleDuration

    let watchModule moduleF label =
        let (xs, data) = moduleF () |> watchCmdCycle
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show label

    let subAgtB () = 
        let A = ("A", 10.0)
        let B = ("B", 8.0)
        let C = ("C", 0.0)

        let (xs, data) = Modules.sub A B C |> watchCmdCycle
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "sub with A > B"

    let subAltB () = 
        let A = ("A", 2.0)
        let B = ("B", 8.0)
        let C = ("C", 0.0)

        let (xs,data) = Modules.sub A B C |> watchCmdCycle
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "sub with A < B"

    let add () = 
        let A = ("A", 2.0)
        let B = ("B", 8.0)
        let C = ("C", 0.0)

        let (xs,data) = Modules.add A B C |> watchCmdCycle
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "add"

    let mul () = 
        let A = ("A", 2.0)
        let B = ("B", 8.0)
        let C = ("C", 0.0)

        let (xs,data) = Modules.mul A B C |> watchCmdCycle
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "mul"

    let div () = 
        let A = ("A", 8.0)
        let B = ("B", 2.0)
        let C = ("C", 0.0)

        let (xs,data) = Modules.div A B C |> watchCmdCycle
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "div"


    let sqrt () =
        let A = ("A", 16.0)
        let B = ("B", 0.0)

        let (xs,data) = Modules.sqrt A B |> watchCmdCycle
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "sqrt"

    let clock phases =
        let duration = 120.0
        let formula = [1..phases] |> List.map (fun _ -> []) // empty steps to just see clock phases

        let (xs,data) = Simulator.watch duration formula
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "clock"

    let cmp () =
        let A = ("A", 2.0)
        let B = ("B", 5.0)

        let (xs,data) = Modules.cmp A B |> watchCmd 90.0
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "cmp"

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
        let (xs, data) = Simulator.watch duration formula
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "ifGt A > B -> C := A; A < B -> C := B"

