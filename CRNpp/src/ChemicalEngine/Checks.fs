// Author: Roar Nind Steffensen, 13/06/2023

namespace ChemicalEngine

module Checks =
    open Reaction
    open FsCheck

    type Concentration = Con of float

    let private runCmd = Command.toCRN
                        >> Simulator.simulate
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
        (conc A > conc B + 1.0) ==>
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

    let cmpCheck (Con(c1)) (Con(c2)) =
        let A = ("A", c1)
        let B = ("B", c2)
        
        let formula = [[Modules.cmp A B]]
        let duration = Simulator.approxCycleDuration * Clock.stepPeriodF * 3.0

        let _,data = (Simulator.watch duration formula)
        
        if conc A > conc B then
            data
            |> List.find (fun (n, cs) -> n = name Modules.cmpYEltX)
            |> (fun (n, c) -> floatEquals (List.last c) 1.0)
        elif conc A < conc B then
            data
            |> List.find (fun (n, cs) -> n = name Modules.cmpYEgtX)
            |> (fun (n, c) -> floatEquals (List.last c) 1.0)
        else
            data
            |> List.filter (fun (n, cs) -> n = name Modules.cmpXEgtY || n = name Modules.cmpYEgtX)
            |> List.map (fun (n, cs) -> List.last cs)
            |> List.forall (fun c -> floatEquals c 1.0)
    
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
                                        |> Gen.map (round >> Con)
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
        checkQuick "cmp" cmpCheck
        printfn "Module checks done"

        printfn $"{nameof ChemicalEngine} checks done"
