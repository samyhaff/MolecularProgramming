module Interpreter

open ChemicalEngine
open Ast

let getInitialConcentrations (ast: Crn) : Map<string, float> =
    let rec getInitialConcentrationsRoot ast env =
        match ast with
        | [] -> env
        | Conc(S s, c)::rest ->
            getInitialConcentrationsRoot rest (Map.add s c env)
        | Step(commands)::rest ->
            let rec getUnboundSpecies commands env =
               match commands with
               | [] -> env
               | Sqrt(_, S s)::rest
               | Cmp(_, S s)::rest
               | Load(_, S s)::rest
               | Add(_, _, S s)::rest
               | Sub(_, _, S s)::rest
               | Mul(_, _, S s)::rest
               | Div(_, _, S s)::rest ->
                  if not (Map.containsKey s env) then
                     getUnboundSpecies rest (Map.add s 0.0 env)
                  else
                     getUnboundSpecies rest env
               | Rxn(reactants, products, _)::rest ->
                   let rec getUnboundSpeciesChemicals chemicals env =
                     match chemicals with
                     | [] -> env
                     | S s::rest ->
                        if not (Map.containsKey s env) then
                            getUnboundSpeciesChemicals rest (Map.add s 0.0 env)
                        else
                            getUnboundSpeciesChemicals rest env
                   let env' = getUnboundSpeciesChemicals products env
                   getUnboundSpecies rest env'
               | Ifgt(commands)::rest
               | Iflt(commands)::rest
               | Ifge(commands)::rest
               | Ifle(commands)::rest
               | Ifeq(commands)::rest ->
                  getUnboundSpecies commands env |> getUnboundSpecies rest
            getInitialConcentrationsRoot rest (getUnboundSpecies commands env)
    in getInitialConcentrationsRoot ast Map.empty

let convertAstToFormula (ast: Crn) :Reaction.Formula =
    let env = getInitialConcentrations ast
    let rec convertRoot (ast: Crn) : Reaction.Formula =
        match ast with
        | [] -> []
        | Conc(s, c)::rest ->
            convertRoot rest
        | Step(commands)::rest ->
            let rec convertCommands (commands: Command list) : Reaction.Step =
                match commands with
                    | [] -> []
                    | Sqrt(S s1, S s2)::rest
                    | Cmp(S s1, S s2)::rest
                    | Load(S s1, S s2)::rest as (command::_ : Command list) ->
                        let c1 = Map.find s1 env
                        let c2 = Map.find s2 env
                        match command with
                        | Sqrt(_, _) -> (Modules.sqrt (s1, c1) (s2, c2))::(convertCommands rest)
                        | Cmp(_, _) -> (Modules.cmp (s1, c1) (s2, c2))::(convertCommands rest)
                        | Load(_, _) -> (Modules.ld (s1, c1) (s2, c2))::(convertCommands rest)
                        | _ -> failwith "Impossible"
                    | Add(S s1, S s2, S s3)::rest
                    | Sub(S s1, S s2, S s3)::rest
                    | Mul(S s1, S s2, S s3)::rest
                    | Div(S s1, S s2, S s3)::rest as (command::_ : Command list) ->
                        let c1 = Map.find s1 env
                        let c2 = Map.find s2 env
                        let c3 = Map.find s3 env
                        match command with
                        | Add(_, _, _) -> (Modules.add (s1, c1) (s2, c2) (s3, c3))::(convertCommands rest)
                        | Sub(_, _, _) -> (Modules.sub (s1, c1) (s2, c2) (s3, c3))::(convertCommands rest)
                        | Mul(_, _, _) -> (Modules.mul (s1, c1) (s2, c2) (s3, c3))::(convertCommands rest)
                        | Div(_, _, _) -> (Modules.div (s1, c1) (s2, c2) (s3, c3))::(convertCommands rest)
                        | _ -> failwith "Impossible"
                    | Ifgt(commands)::rest
                    | Iflt(commands)::rest
                    | Ifge(commands)::rest
                    | Ifle(commands)::rest
                    | Ifeq(commands)::rest as (command::_ : Command list) ->
                        let convertedCommands = convertCommands commands
                        match command with
                        | Ifgt(_) -> (Modules.ifGt convertedCommands)::(convertCommands rest)
                        | Iflt(_) -> (Modules.ifLt convertedCommands)::(convertCommands rest)
                        | Ifeq(_) -> (Modules.ifEq convertedCommands)::(convertCommands rest)
                        | Ifge(_) -> (Modules.ifGe convertedCommands)::(convertCommands rest)
                        | Ifle(_) -> (Modules.ifLe convertedCommands)::(convertCommands rest)
                        | _ -> failwith "Impossible"
                    | Rxn(reactants, products, rate)::rest ->
                        let name = List.map (fun (S s) -> s)
                        let r = name reactants
                        let p = name products
                        let concentrationsReactants = List.map (fun (S s) -> Map.find s env) reactants
                        let concentrationsProducts = List.map (fun (S s) -> Map.find s env) products
                        (Modules.rxn
                            (List.zip r concentrationsReactants) rate (List.zip p concentrationsProducts))
                        ::(convertCommands rest)
                    | _ -> failwith "Impossible"
            let step = convertCommands commands
            in step::(convertRoot rest)
    in convertRoot ast

let eval formula filter duration =
    Simulator.watchFiltered duration filter formula |> Simulator.shrinkData
