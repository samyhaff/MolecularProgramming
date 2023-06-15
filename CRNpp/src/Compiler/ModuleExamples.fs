module ModuleExamples

open ChemicalEngine
open Reaction
open Rendering.Plotting

let subAgtB () = 
    let A = ("A", 10.0)
    let B = ("B", 8.0)
    let C = ("C", 0.0)

    let n = 200
    let data = Modules.sub A B C |> Simulator.watch n

    let xs = [1..n] |> List.map float
    data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "sub with A > B"

let subAltB () = 
    let A = ("A", 2.0)
    let B = ("B", 8.0)
    let C = ("C", 0.0)

    let n = 100
    let data = Modules.sub A B C |> Simulator.watch n

    let xs = [1..n] |> List.map float
    data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "sub with A < B"

let add () = 
    let A = ("A", 2.0)
    let B = ("B", 8.0)
    let C = ("C", 0.0)

    let n = 100
    let data = Modules.add A B C |> Simulator.watch n

    let xs = [1..n] |> List.map float
    data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "add"

let mul () = 
    let A = ("A", 2.0)
    let B = ("B", 8.0)
    let C = ("C", 0.0)

    let n = 100
    let data = Modules.mul A B C |> Simulator.watch n

    let xs = [1..n] |> List.map float
    data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "mul"

let div () = 
    let A = ("A", 8.0)
    let B = ("B", 2.0)
    let C = ("C", 0.0)

    let n = 100
    let data = Modules.div A B C |> Simulator.watch n

    let xs = [1..n] |> List.map float
    data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "div"


let sqrt () =
    let A = ("A", 16.0)
    let B = ("B", 0.0)

    let n = 100
    let data = Modules.sqrt A B |> Simulator.watch n

    let xs = [1..n] |> List.map float
    data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "sqrt"