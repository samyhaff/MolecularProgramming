module Interpreter

open ChemicalEngine
open Ast
open Rendering.Plotting

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
               | Ifgt(commands)::rest
               | Iflt(commands)::rest
               | Ifge(commands)::rest
               | Ifle(commands)::rest
               | Ifeq(commands)::rest ->
                  getUnboundSpecies commands env
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
                    | _ -> failwith "Impossible"
            let step = convertCommands commands
            in step::(convertRoot rest)
    in convertRoot ast

let showDuration duration names title formula =
    let filter = Simulator.onlyByNames names
    let (xs, data) = Simulator.watchFiltered duration filter formula |> Simulator.shrinkData
    data |> List.map (fun (n, ys) -> line xs ys n) |> showLabelledPlots title "time" "concentrations" (600, 600)

let run (ast: Crn) (duration: float) (species: string list) (title: string) =
    convertAstToFormula ast |> showDuration duration species title
