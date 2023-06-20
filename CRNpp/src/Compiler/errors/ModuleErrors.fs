// Author: Roar Nind Steffensen, 19/06/2023

namespace Errors

module Modules =
    open ChemicalEngine
    open Reaction
    open Rendering.Plotting

    let private runCmd = 
        Command.toCRN
        >> Simulator.simulateFast
        >> Seq.skip (Simulator.stepsInDuration Simulator.approxCycleDuration)
        >> Seq.head

    let eval resultSpecies = 
        runCmd 
        >> snd 
        >> Map.find (name resultSpecies) 
        >> conc

    let calcGrid f range =
        List.map (fun b -> List.map (fun a -> f a b) range) range

    let plotError title error =
        let range = [1..50] |> List.map float
        let z = calcGrid error range
        surface range range z "A" "B" "error" |> showPlot title

    let A c = ("A", c)
    let B c= ("B", c)
    let C = ("C", 0.0)

    let private addError a b =
        let simulated = Modules.add (A a) (B b) C |> eval C
        let expected = a + b
        abs (expected - simulated)
    let add () = plotError "add error" addError

    let private subError a b =
        let simulated = Modules.sub (A a) (B b) C |> eval C
        let expected = if a > b then a - b else 0.0
        abs (expected - simulated)
    let sub () =
        plotError "sub error" subError

    let subAGtB () =
        plotError "sub error on domain: A >= B" (fun a b -> if a >= b then subError a b else 0.0)

    let subOneOff () =
        let range = [1..500] |> List.map float
        let data = List.map (fun x -> subError (x+1.0) x) range
        line range data "" |> showPlot "sub: (x+1) - x"

    let subMinus1 () =
        let range = [1..500] |> List.map float
        let data = List.map (fun x -> subError x 1.0) range
        line range data "" |> showPlot "sub: x - 1"



    let private mulError a b =
        let simulated = Modules.mul (A a) (B b) C |> eval C
        let expected = a * b
        abs (expected - simulated)
    let mul () = plotError "mul error" mulError

    let private divError a b =
        let simulated = Modules.div (A a) (B b) C |> eval C
        let expected = a / b
        abs (expected - simulated)
    let div () = plotError "div error" divError