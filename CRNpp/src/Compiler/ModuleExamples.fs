module ModuleExamples

open ChemicalEngine
open Reaction
open Rendering.Plotting

let subAgtB () = 
    let A = S("A", 10.0, 1)
    let B = S("B", 8.0, 1)
    let C = S("C", 0.0, 1)

    let n = 100
    let data = Modules.sub A B C |> Simulator.watch n

    let xs = [1..n] |> List.map float
    data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "sub with A > B"

let subAltB () = 
    let A = S("A", 2.0, 1)
    let B = S("B", 8.0, 1)
    let C = S("C", 0.0, 1)

    let n = 100
    let data = Modules.sub A B C |> Simulator.watch n

    let xs = [1..n] |> List.map float
    data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "sub with A < B"

let add () = 
    let A = S("A", 2.0, 1)
    let B = S("B", 8.0, 1)
    let C = S("C", 0.0, 1)

    let n = 100
    let data = Modules.add A B C |> Simulator.watch n

    let xs = [1..n] |> List.map float
    data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "add"

let mul () = 
    let A = S("A", 2.0, 1)
    let B = S("B", 8.0, 1)
    let C = S("C", 0.0, 1)

    let n = 100
    let data = Modules.mul A B C |> Simulator.watch n

    let xs = [1..n] |> List.map float
    data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "mul"

let div () = 
    let A = S("A", 8.0, 1)
    let B = S("B", 2.0, 1)
    let C = S("C", 0.0, 1)

    let n = 100
    let data = Modules.div A B C |> Simulator.watch n

    let xs = [1..n] |> List.map float
    data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "div"