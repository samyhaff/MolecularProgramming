namespace Errors

module Modules =
    open ChemicalEngine
    open Reaction
    open Rendering.Plotting

    let private runCmd = 
        Command.toCRN
        >> Simulator.simulate
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

    let add () =
        plotError "add error" (fun a b ->
            let simulated = Modules.add (A a) (B b) C |> eval C
            let expected = a + b
            abs (expected - simulated)
        )

    let sub () =
        plotError "sub error" (fun a b ->
            let simulated = Modules.sub (A a) (B b) C |> eval C
            let expected = if a > b then a - b else 0.0
            abs (expected - simulated)
        )

    let subAGtB () =
        plotError "sub error on domain: A >= B" (fun a b ->
            let simulated = if a >= b then Modules.sub (A a) (B b) C |> eval C else 0.0
            let expected = if a > b then a - b else 0.0
            abs (expected - simulated)
        )

    let mul () =
        plotError "mul error" (fun a b ->
            let simulated = Modules.mul (A a) (B b) C |> eval C
            let expected = a * b
            abs (expected - simulated)
        )

    let div () =
        plotError "div error" (fun a b ->
            let simulated = Modules.div (A a) (B b) C |> eval C
            let expected = a / b
            abs (expected - simulated)
        )