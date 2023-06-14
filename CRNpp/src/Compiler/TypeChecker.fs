module TypeChecker

open Ast

let rec checkModule (command: Command) =
   match command with
   | Add(s1, s2, s3)
   | Sub(s1, s2, s3)
   | Mul(s1, s2, s3)
   | Div(s1, s2, s3) -> s3 <> s1 && s3 <> s2
   | Load(s1, s2)
   | Cmp(s1, s2)
   | Sqrt(s1, s2) -> s1 <> s2
   | Iflt(commands)
   | Ifle(commands)
   | Ifeq(commands)
   | Ifge(commands)
   | Ifeq(commands)
   | Ifgt(commands) -> List.forall checkModule commands

let rec checkProgram (program: Crn) =
   match program with
   | [] -> true
   | Conc(_) :: rest -> checkProgram rest
   | Step(commands) :: rest ->
      List.forall checkModule commands && checkProgram rest
