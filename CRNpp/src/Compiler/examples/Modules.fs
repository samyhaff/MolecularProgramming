// Author: Roar Nind Steffensen, 14/06/2023

namespace Examples

module Modules =

    open ChemicalEngine
    open Reaction
    open Rendering.Plotting

    let private resF = Simulator.constRes

    let private n = 20.0
    let steps i = float i * resF i
    let private xs = Seq.initInfinite steps 
                    |> Seq.takeWhile (fun i -> i < n)
                    |> Seq.toList

    // let private watch = Simulator.watch resF (List.length xs)
    let private watch = FakeClockSimulator.watchConstRes n
    let private cmdToFormula cmd :Formula = [[cmd]]
    let private watchCmd = cmdToFormula >> watch

    let watchModule moduleF label =
        let data = moduleF () |> watchCmd
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show label

    let subAgtB () = 
        let A = ("A", 10.0)
        let B = ("B", 8.0)
        let C = ("C", 0.0)

        let data = Modules.sub A B C |> watchCmd
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "sub with A > B"

    let subAltB () = 
        let A = ("A", 2.0)
        let B = ("B", 8.0)
        let C = ("C", 0.0)

        let data = Modules.sub A B C |> watchCmd
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "sub with A < B"

    let add () = 
        let A = ("A", 2.0)
        let B = ("B", 8.0)
        let C = ("C", 0.0)

        let data = Modules.add A B C |> watchCmd
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "add"

    let mul () = 
        let A = ("A", 2.0)
        let B = ("B", 8.0)
        let C = ("C", 0.0)

        let data = Modules.mul A B C |> watchCmd
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "mul"

    let div () = 
        let A = ("A", 8.0)
        let B = ("B", 2.0)
        let C = ("C", 0.0)

        let data = Modules.div A B C |> watchCmd
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "div"


    let sqrt () =
        let A = ("A", 16.0)
        let B = ("B", 0.0)

        let data = Modules.sqrt A B |> watchCmd
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "sqrt"

    let clock phases =
        let crn = Modules.clock phases
        printfn "reactions: %A" (fst crn)
        printfn "solution: %A" (snd crn)

        let duration = 120.0
        let xs = Seq.initInfinite Simulator.constResTime
                    |> Seq.takeWhile (fun i -> i <= duration)
                    |> Seq.toList

        Simulator.watch resF (List.length xs) crn 
            |> FakeClockSimulator.filterNames [Clock.clockSpeciesName 0]
            |> List.map (fun (n, ys) -> scatter xs ys n) 
            |> show "clock"

    let cmp () =
        let A = ("A", 2.0)
        let B = ("B", 5.0)

        let formula = Modules.cmp A B |> cmdToFormula

        let duration = 90.0
        let xs = Seq.initInfinite steps 
                    |> Seq.takeWhile (fun i -> i < duration)
                    |> Seq.toList
        let data = FakeClockSimulator.watchConstRes duration formula
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
        let xs = Seq.initInfinite steps 
                    |> Seq.takeWhile (fun i -> i < duration)
                    |> Seq.toList
        let data = FakeClockSimulator.watchConstRes duration formula
        data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "ifGt A > B -> C := A; A < B -> C := B"

