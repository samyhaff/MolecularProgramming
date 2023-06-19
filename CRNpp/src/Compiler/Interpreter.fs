module Interpreter

open ChemicalEngine
open Ast

let getInitialConcentrations (ast: Crn) : Map<string, float> =
   let rec getInitialConcentrationsRoot ast env =
      match ast with
      | [] -> env
      | Conc(s, c)::rest ->
         getInitialConcentrationsRoot rest (Map.add s c env)
      | Step(commands)::rest ->
         let rec getUnboundSpecies commands env =
            match commands with
            | [] -> env
            | Sqrt(_, s)::rest
            | Cmp(_, s)::rest
            | Load(_, s)::rest
            | Add(_, _, s)::rest
            | Sub(_, _, s)::rest
            | Mul(_, _, s)::rest
            | Div(_, _, s)::rest ->
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
               | Sqrt(s1, s2)::rest
               | Cmp(s1, s2)::rest
               | Load(s1, s2)::rest as (command::_ : Command list) ->
                  let c1 = Map.find s1 env
                  let c2 = Map.find s1 env
                  match command with
                  | Sqrt(_, _) -> (Modules.sqrt (s1, c1) (s2, c2))::(convertCommands rest)
                  | Cmp(_, _) -> (Modules.cmp (s1, c1) (s2, c2))::(convertCommands rest)
                  | Load(_, _) -> (Modules.ld (s1, c1) (s2, c2))::(convertCommands rest)
                  | _ -> failwith "Impossible"
               | Add(s1, s2, s3)::rest
               | Sub(s1, s2, s3)::rest
               | Mul(s1, s2, s3)::rest
               | Div(s1, s2, s3)::rest as (command::_ : Command list) ->
                  let c1 = Map.find s1 env
                  let c2 = Map.find s1 env
                  let c3 = Map.find s1 env
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
                  | _ -> failwith "Missing cases"
         let step = convertCommands commands
         in step::(convertRoot rest)
   in convertRoot ast
