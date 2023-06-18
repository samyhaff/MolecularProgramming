// Author: Roar Nind Steffensen, 13/06/2023

namespace ChemicalEngine

module Checks =
    open Reaction
    open FsCheck

    type Concentration = Con of float

    let private cmdToFormula cmd :Formula = [[cmd]]
    let private runCmd = cmdToFormula
                        >> Simulator.simulateFormula
                        >> snd
                        >> Seq.skip (Simulator.stepsInDuration Simulator.approxCycleDuration)
                        >> Seq.head

    let floatEquals f1 f2 =
        abs (f2-f1) < 0.5

    let verify (name:Name) (crn:CRN) (expected:float) =
        Map.find name (snd crn) |> conc |> floatEquals expected

    let ldCheck (Con(c1)) (Con(c2)) =
        let A = ("A", c1)
        let B = ("B", c2)

        let crn = Modules.ld A B |> runCmd
        verify (name A) crn (conc A) &&
        verify (name B) crn (conc A)

    let addCheck (Con(c1)) (Con(c2)) (Con(c3)) =
        let A = ("A", c1)
        let B = ("B", c2)
        let C = ("C", c3)

        let crn = Modules.add A B C |> runCmd
        verify (name A) crn (conc A) && 
        verify (name B) crn (conc B) && 
        verify (name C) crn (conc A + conc B)

    let subAgtBCheck (Con(c1)) (Con(c2)) (Con(c3)) =
        let A = ("A", c1)
        let B = ("B", c2)
        let C = ("C", c3)

        let crn = Modules.sub A B C |> runCmd
        (conc A > conc B + 2.0) ==>
            (verify (name A) crn (conc A) && 
            verify (name B) crn (conc B) && 
            verify (name C) crn (conc A - conc B))


    let mulCheck (Con(c1)) (Con(c2)) (Con(c3)) =
        let A = ("A", c1)
        let B = ("B", c2)
        let C = ("C", c3)

        let crn = Modules.mul A B C |> runCmd
        verify (name A) crn (conc A) &&
        verify (name B) crn (conc B) &&
        verify (name C) crn (conc A * conc B)

    let divCheck (Con(c1)) (Con(c2)) (Con(c3)) =
        let A = ("A", c1)
        let B = ("B", c2)
        let C = ("C", c3)

        let crn = Modules.div A B C |> runCmd
        (conc B > 0.5) ==>
            (verify (name A) crn (conc A) &&
            verify (name B) crn (conc B) &&
            verify (name C) crn (conc A / conc B))

    let sqrtCheck (Con(c1)) (Con(c2)) =
        let A = ("A", c1)
        let B = ("B", c2)

        let crn = Modules.sqrt A B |> runCmd
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
