namespace ChemicalEngine

module Checks =
    open Reaction
    open FsCheck

    type Concentration = Con of float

    let CRNtolerance = 0.001

    let floatEquals f1 f2 =
        abs (f2-f1) < 0.05

    let verify (name:Name) (crn:CRN) (expected:float) =
        Map.find name (snd crn) |> conc |> floatEquals expected
    let concIn (crn:CRN) (s:Chemical) = 
        Map.find (name s) (snd crn) |> conc

    let ldCheck (Con(c1)) (Con(c2)) =
        let A = S("A", c1, 1)
        let B = S("B", c2, 1)

        let crn = Modules.ld A B |> Simulator.runToStable CRNtolerance
        verify (name A) crn (conc A) &&
        verify (name B) crn (conc A)

    let addCheck (Con(c1)) (Con(c2)) (Con(c3)) =
        let A = S("A", c1, 1)
        let B = S("B", c2, 1)
        let C = S("C", c3, 1)

        let crn = Modules.add A B C |> Simulator.runToStable CRNtolerance
        verify (name A) crn (conc A) && 
        verify (name B) crn (conc B) && 
        verify (name C) crn (conc A + conc B)

    // todo add when sub module has fixed the oscilating result
    let subCheck (Con(c1)) (Con(c2)) (Con(c3)) =
        let A = S("A", c1, 1)
        let B = S("B", c2, 1)
        let C = S("C", c3, 1)

        let crn = Modules.sub A B C |> Simulator.runToStable CRNtolerance
        verify (name A) crn (conc A) && 
        verify (name B) crn (conc B) && 
        verify (name C) crn (if conc A > conc B then (conc A - conc B) else 0.0)


    let mulCheck (Con(c1)) (Con(c2)) (Con(c3)) =
        let A = S("A", c1, 1)
        let B = S("B", c2, 1)
        let C = S("C", c3, 1)

        let crn = Modules.mul A B C |> Simulator.runToStable CRNtolerance
        verify (name A) crn (conc A) &&
        verify (name B) crn (conc B) &&
        verify (name C) crn (conc A * conc B)

    // todo add when div module has fixed the oscilating result
    // let divCheck (Con(c1)) (Con(c2)) (Con(c3)) =
    //     let A = S("A", c1, 1)
    //     let B = S("B", c2, 1)
    //     let C = S("C", c3, 1)

    //     let crn = Modules.div A B C |> Simulator.runToStable CRNtolerance
    //     verify (name A) crn (conc A) &&
    //     verify (name B) crn (conc B) &&
    //     verify (name C) crn (conc A / conc B)


    type CustomGenerators =
        static member float() =
            {
                new Arbitrary<float>() with
                override x.Generator = Arb.generate<NormalFloat>
                                        |> Gen.map NormalFloat.op_Explicit
            }
        static member concentration() =
            {
                new Arbitrary<Concentration>() with
                override x.Generator = Arb.generate<NormalFloat>
                                        |> Gen.map NormalFloat.op_Explicit
                                        |> Gen.where ((<)0.0)
                                        |> Gen.map Con
            }

    let runAll () =
        printfn $"Running {nameof ChemicalEngine} checks"
        let checkQuick lbl prop = Check.One ({Config.Quick with Name = lbl}, prop)
        Arb.register<CustomGenerators>() |> ignore

        printfn "Checking modules:"
        checkQuick "ld" ldCheck
        checkQuick "add" addCheck
        checkQuick "sub" subCheck
        checkQuick "mul" mulCheck
        printfn "Module checks done"

        printfn $"{nameof ChemicalEngine} checks done"
