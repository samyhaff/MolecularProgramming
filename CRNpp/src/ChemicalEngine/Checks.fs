// Author: Roar Nind Steffensen, 13/06/2023

namespace ChemicalEngine

module Checks =
    open Reaction
    open FsCheck

    type Concentration = Con of float
    type SubInputs = Sub of Concentration * Concentration
    type DivInputs = Div of Concentration * Concentration

    let private runCmd = Command.toCRN
                        >> Simulator.simulateFast
                        >> Seq.skip (Simulator.stepsInDuration Simulator.approxCycleDuration)
                        >> Seq.head

    let floatEquals f1 f2 =
        abs (f2-f1) < 0.5

    let verify (name:Name) (crn:CRN) (expected:float) =
        Map.find name (snd crn) |> conc |> floatEquals expected

    let ldCheck (Con c1) (Con c2) =
        let A = ("A", c1)
        let B = ("B", c2)

        let crn = Modules.ld A B |> runCmd
        verify (name A) crn (conc A) &&
        verify (name B) crn (conc A)

    let addCheck (Con c1) (Con c2) =
        let A = ("A", c1)
        let B = ("B", c2)
        let C = ("C", 0.0)

        let crn = Modules.add A B C |> runCmd
        verify (name A) crn (conc A) && 
        verify (name B) crn (conc B) && 
        verify (name C) crn (conc A + conc B)

    let subAgtBCheck (Sub(Con c1, Con c2)) =
        let A = ("A", c1)
        let B = ("B", c2)
        let C = ("C", 0.0)

        let crn = Modules.sub A B C |> runCmd

        verify (name A) crn (conc A) && 
        verify (name B) crn (conc B) && 
        verify (name C) crn (conc A - conc B)


    let mulCheck (Con c1) (Con c2) =
        let A = ("A", c1)
        let B = ("B", c2)
        let C = ("C", 0.0)

        let crn = Modules.mul A B C |> runCmd
        verify (name A) crn (conc A) &&
        verify (name B) crn (conc B) &&
        verify (name C) crn (conc A * conc B)

    let divCheck (Div(Con c1, Con c2)) =
        let A = ("A", c1)
        let B = ("B", c2)
        let C = ("C", 0.0)

        let crn = Modules.div A B C |> runCmd

        verify (name A) crn (conc A) &&
        verify (name B) crn (conc B) &&
        verify (name C) crn (conc A / conc B)

    let sqrtCheck (Con c1) =
        let A = ("A", c1)
        let B = ("B", 0.0)

        let crn = Modules.sqrt A B |> runCmd
        verify (name A) crn (conc A) &&
        verify (name B) crn (conc A |> sqrt)

    let cmpCheck (Con(c1)) (Con(c2)) =
        let A = ("A", c1)
        let B = ("B", c2)
        
        let formula = [[Modules.cmp A B]]
        let duration = Simulator.approxCycleDuration * Clock.stepPeriodF * 1.9 // end of second phase, before next cycle begins
        
        let crn = Simulator.simulateFormula formula |> snd |> Seq.skip (Simulator.stepsInDuration duration) |> Seq.head
        if conc A > conc B then
            verify (name Modules.cmpYEltX) crn 1.0
        elif conc A < conc B then
            verify (name Modules.cmpXEltY) crn 1.0
        else
            verify (name Modules.cmpXEgtY) crn 1.0 &&
            verify (name Modules.cmpYEgtX) crn 1.0
    
    type CustomGenerators =
        static member float() =
            {
                new Arbitrary<float>() with
                override x.Generator = 
                    Arb.generate<NormalFloat>
                    |> Gen.map NormalFloat.op_Explicit
            }
        static member concentration() =
            {
                new Arbitrary<Concentration>() with
                override x.Generator = 
                    Arb.generate<NormalFloat>
                        |> Gen.map NormalFloat.op_Explicit
                        |> Gen.where ((<)0.0)
                        |> Gen.map (round >> Con)
            }
        static member sub() =
            {
                new Arbitrary<SubInputs>() with
                override x.Generator = 
                    gen { 
                        let! (Con a) = Arb.generate<Concentration>
                        let bMax = max 0 ((int a) - 2)
                        let! b = Gen.choose(0, bMax) // restrict domain to b + 2 < a
                        return Sub(Con a, Con b)
                    }
            }
        static member div() =
            {
                new Arbitrary<DivInputs>() with
                override x.Generator = 
                    Arb.generate<Concentration>
                        |> Gen.map (fun c -> Div (c,c))
                        // avoid division by zero or near-zero
                        |> Gen.where (fun (Div (_, Con b)) -> b > 0.2)
            }


    let runAll () =
        printfn $"Running {nameof ChemicalEngine} checks"
        let checkQuick lbl prop = Check.One ({Config.Quick with Name = lbl}, prop)
        Arb.register<CustomGenerators>() |> ignore

        checkQuick "ld module" ldCheck
        checkQuick "add module" addCheck
        checkQuick "sub module" subAgtBCheck
        checkQuick "mul module" mulCheck
        checkQuick "div module" divCheck
        checkQuick "sqrt module" sqrtCheck
        checkQuick "cmp module" cmpCheck

        printfn $"{nameof ChemicalEngine} checks done"
