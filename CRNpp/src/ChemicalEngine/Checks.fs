namespace ChemicalEngine

module Checks =
    open Reaction
    open FsCheck

    let floatEquals f1 f2 =
        abs (f2-f1) < 1e-5

    let multiplicationCheck (c1:float) (c2:float) =
        let A = C("A", c1, 1)
        let B = C("B", c2, 1)
        let C = C("C", 0, 1)

        let reactions :Reaction list= [
            (["A";"B"], 1, ["A";"B";"C"]);
            (["C"], 1, [])
        ]

        let crn = toCRN reactions [A;B;C] |> Simulator.run
        let (C(_,result,_)) = Map.find "C" (snd crn)
        floatEquals result (c1*c2)

    type CustomGenerators =
        static member float() =
            {
                new Arbitrary<float>() with
                override x.Generator = Arb.generate<NormalFloat>
                                        |> Gen.map NormalFloat.op_Explicit
            }

    let runAll () =
        printfn $"Running {nameof ChemicalEngine} checks"
        let checkQuick lbl prop = Check.One ({Config.Quick with Name = lbl}, prop)

        Arb.register<CustomGenerators>() |> ignore
        checkQuick "Multiplication by CRN" multiplicationCheck
        printfn "Checks done"
