namespace ChemicalEngine

module Checks =
    open Reaction
    open FsCheck

    type Concentration = Con of float

    let CRNtolerance = 0.0001
    let CRNresolution = 0.01

    let run = Simulator.runToStable CRNresolution CRNtolerance

    let floatEquals f1 f2 =
        abs (f2-f1) < 0.5

    let verify (name:Name) (crn:CRN) (expected:float) =
        Map.find name (snd crn) |> conc |> floatEquals expected
    let concIn (crn:CRN) (s:Species) = 
        Map.find (name s) (snd crn) |> conc

    let ldCheck (Con(c1)) (Con(c2)) =
        let A = ("A", c1)
        let B = ("B", c2)

        let crn = Modules.ld A B |> run
        verify (name A) crn (conc A) &&
        verify (name B) crn (conc A)

    let addCheck (Con(c1)) (Con(c2)) (Con(c3)) =
        let A = ("A", c1)
        let B = ("B", c2)
        let C = ("C", c3)

        let crn = Modules.add A B C |> run
        verify (name A) crn (conc A) && 
        verify (name B) crn (conc B) && 
        verify (name C) crn (conc A + conc B)

    let subAgtBCheck (Con(c1)) (Con(c2)) (Con(c3)) =
        let A = ("A", c1)
        let B = ("B", c2)
        let C = ("C", c3)

        let crn = Modules.sub A B C |> run
        (conc A > conc B + 1.0) ==>
            (verify (name A) crn (conc A) && 
            verify (name B) crn (conc B) && 
            verify (name C) crn (conc A - conc B))


    let mulCheck (Con(c1)) (Con(c2)) (Con(c3)) =
        let A = ("A", c1)
        let B = ("B", c2)
        let C = ("C", c3)

        let crn = Modules.mul A B C |> run
        verify (name A) crn (conc A) &&
        verify (name B) crn (conc B) &&
        verify (name C) crn (conc A * conc B)

    let divCheck (Con(c1)) (Con(c2)) (Con(c3)) =
        let A = ("A", c1)
        let B = ("B", c2)
        let C = ("C", c3)

        let crn = Modules.div A B C |> run
        verify (name A) crn (conc A) &&
        verify (name B) crn (conc B) &&
        verify (name C) crn (conc A / conc B)

    let sqrtCheck (Con(c1)) (Con(c2)) =
        let A = ("A", c1)
        let B = ("B", c2)

        let crn = Modules.sqrt A B |> run
        verify (name A) crn (conc A) &&
        verify (name B) crn (conc A |> sqrt)

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
        checkQuick "sub" subAgtBCheck
        checkQuick "mul" mulCheck
        checkQuick "div" divCheck
        checkQuick "sqrt" sqrtCheck
        printfn "Module checks done"

        printfn $"{nameof ChemicalEngine} checks done"
