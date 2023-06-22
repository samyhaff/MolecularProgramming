// Author: Roar Nind Steffensen, 21/06/2023

namespace Checks

module AstGenerator =
    open FsCheck
    open Ast

    let private composable (commands,_)  = 
        let rec extractInputs commands =
            List.fold (fun acc cmd -> 
                match cmd with 
                | Load (s,_) | Sqrt (s,_) -> s::acc
                | Add (s1,s2,_) | Sub (s1,s2,_) | Mul (s1,s2,_) 
                | Div (s1,s2,_) | Cmp(s1,s2) ->  s1::s2::acc
                | Rxn (reactants, _, _) -> reactants @ acc
                | Ifeq cmds | Ifgt cmds | Ifge cmds | Iflt cmds | Ifle cmds -> extractInputs cmds
            ) [] commands

        let rec extractOutputs commands =
            List.fold (fun acc cmd -> 
                match cmd with 
                | Load (_,s) | Sqrt (_,s) 
                | Add (_,_,s) | Sub (_,_,s) | Mul (_,_,s) 
                | Div (_,_,s) ->  s::acc
                | Cmp _ -> acc
                | Rxn (_, products, _) -> products @ acc
                | Ifeq cmds | Ifgt cmds | Ifge cmds | Iflt cmds | Ifle cmds -> extractOutputs cmds
            ) [] commands

        let inputs = extractInputs commands
        let outputs = extractOutputs commands
        not <| List.exists (fun output -> List.contains output inputs) outputs

    let private newSpeciesG env = 
        // creates a new label of 5 random characters
        let chars = [97..122] |> List.map (char>>Gen.constant) |> Gen.oneof
        Gen.arrayOfLength 5 chars
        |> Gen.map (fun s -> System.String s)
        |> Gen.map string
        |> Gen.map (fun s -> S s, Set.add s env)

    let private speciesG env = 
        gen { // env is not empty, since we require at least 1 conc[...] in root generator
            let! s = Gen.choose(0, Set.count env - 1)
            return env |> Seq.skip s |> Seq.head
        } |> Gen.map (fun s -> S s, env)

    let private knownOrNewSpeciesG env = 
        Gen.oneof [speciesG env; newSpeciesG env]

    let private twoSpecies gen1 gen2 (env: string Set) :Gen<(species * species) * string Set>=
        gen {
            let! (s1, env1) = gen1 env
            let! (s2, env2) = gen2 env
            return ((s1, s2), Set.union env1 env2)
        }

    let private threeSpecies gen1 gen2 gen3 (env:string Set) :Gen<(species * species * species) * string Set> =
        gen {
            let! (s1, env1) = gen1 env
            let! (s2, env2) = gen2 env
            let! (s3, env3) = gen3 env
            return ((s1, s2, s3), Set.union env1 env2 |> Set.union env3)
        }


    let private knownAndMaybeSpeciesG env = twoSpecies speciesG knownOrNewSpeciesG env
    let private knownAndKnownSpeciesG env = twoSpecies speciesG speciesG env
    let private knownAndKnownAndMaybeSpeciesG env = threeSpecies speciesG speciesG knownOrNewSpeciesG env


    let moduleG gen modC env = gen env |> Gen.map (fun (g, env) -> modC g, env)

//    | Load of species * species
    let private loadG = moduleG knownAndMaybeSpeciesG Load

//    | Add of species * species * species
    let private addG = moduleG knownAndKnownAndMaybeSpeciesG Add

//    | Sub of species * species * species
    let private subG = moduleG knownAndKnownAndMaybeSpeciesG Add

//    | Mul of species * species * species
    let private mulG = moduleG knownAndKnownAndMaybeSpeciesG Add

//    | Div of species * species * species
    let private divG = moduleG knownAndKnownAndMaybeSpeciesG Add

//    | Sqrt of species * species
    let private sqrtG = moduleG knownAndMaybeSpeciesG Sqrt

//    | Cmp of species * species
    let private cmpSpecies = "cmpSpecies"
    let private cmpG env = 
        // adds cmp env as flag for condition generators
        moduleG knownAndKnownSpeciesG Cmp env
        |> Gen.map (fun (cmd, env) -> cmd, Set.add cmpSpecies env)

    let private nestableCommandsG env =
        [loadG; addG; subG; mulG; divG; sqrtG; cmpG]
        |> List.map (fun c -> c env)
        |> Gen.oneof

    let collectEnv gen = 
        gen |> Gen.map (List.fold (fun (cmds, env) (cmd, env') -> cmd::cmds, Set.union env env') ([], Set.empty))

    let private bindCommandsG commandsG modC env = 
        Gen.nonEmptyListOf (commandsG env)
        |> collectEnv
        |> Gen.where composable
        |> Gen.map (fun (cmds, env) -> modC cmds, env)

    let private nestedModuleG = bindCommandsG nestableCommandsG

//    | Ifgt of Command list
    let private ifgtG = nestedModuleG Ifgt 

//    | Ifge of Command list
    let private ifgeG = nestedModuleG Ifge 

//    | Iflt of Command list
    let private ifltG = nestedModuleG Iflt 

//    | Ifle of Command list
    let private ifleG = nestedModuleG Ifle 

//    | Ifeq of Command list
    let private ifeqG = nestedModuleG Ifeq 

// type Root =

//     | Conc of species * float
    let private concG env = 
        gen {
            let! (s, env') = newSpeciesG env
            let! c = Arb.generate<NormalFloat> 
                    |> Gen.map NormalFloat.op_Explicit 
                    |> Gen.where ((<=) 0.0)  // only non-negative concentrations are possible
                    |> Gen.map (fun c -> System.Math.Round(c, 3)) // restrict decimals to avoid parse truncating
            return ((s, c), env')
        } |> Gen.map (fun (sc, env) -> Conc sc, env)

//     | Step of Command list
    let private stepG env =
        let commandG env = 
            [loadG; addG; subG; mulG; divG; sqrtG; cmpG; ifeqG; ifgtG; ifgeG; ifltG; ifleG] 
            |> List.map (fun c -> c env)
            |> Gen.oneof
            |> Gen.where (fun (cmd,_) -> 
                match cmd with 
                | Ifeq _ | Ifgt _ | Ifge _ | Iflt _ | Ifle _ -> Set.contains cmpSpecies env
                | _ -> true)
        in bindCommandsG commandG Step env

    let private crnGenerator :Crn Gen = 
        gen {
            let env = Set.empty
            let! (concs, env) = Gen.nonEmptyListOf (concG env) |> collectEnv
            let! (steps, _) = Gen.nonEmptyListOf (stepG env) |> collectEnv
            return concs @ steps
        }

    type CustomGenerator =
        static member crn() =
            {
                new Arbitrary<Crn>() with
                override x.Generator = crnGenerator
            }

