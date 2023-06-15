module ModuleExamples

open ChemicalEngine
open Rendering.Plotting

let private CRNresolution = 0.01

let private n = 20

let private xs = Seq.initInfinite (fun i -> float i * CRNresolution) 
                |> Seq.takeWhile (fun x -> x < n)
                |> Seq.toList

let private watch = Simulator.watch CRNresolution (List.length xs)

let watchModule moduleF label =
    let data = moduleF () |> watch
    data |> List.map (fun (n, ys) -> scatter xs ys n) |> show label

let subAgtB () = 
    let A = ("A", 10.0)
    let B = ("B", 8.0)
    let C = ("C", 0.0)

    let data = Modules.sub A B C |> watch
    data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "sub with A > B"

let subAltB () = 
    let A = ("A", 2.0)
    let B = ("B", 8.0)
    let C = ("C", 0.0)

    let data = Modules.sub A B C |> watch
    data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "sub with A < B"

let add () = 
    let A = ("A", 2.0)
    let B = ("B", 8.0)
    let C = ("C", 0.0)

    let data = Modules.add A B C |> watch
    data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "add"

let mul () = 
    let A = ("A", 2.0)
    let B = ("B", 8.0)
    let C = ("C", 0.0)

    let data = Modules.mul A B C |> watch
    data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "mul"

let div () = 
    let A = ("A", 8.0)
    let B = ("B", 2.0)
    let C = ("C", 0.0)

    let data = Modules.div A B C |> watch
    data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "div"


let sqrt () =
    let A = ("A", 16.0)
    let B = ("B", 0.0)

    let data = Modules.sqrt A B |> watch
    data |> List.map (fun (n, ys) -> scatter xs ys n) |> show "sqrt"