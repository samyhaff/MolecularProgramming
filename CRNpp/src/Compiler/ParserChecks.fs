module ParserChecks

open Ast
open Parser
open FsCheck

let rec commandToString (command: Command) : string =
   match command with
   | Load(s1, s2) -> sprintf "ld[%s, %s]" s1 s2
   | Cmp(s1, s2) -> sprintf "cmp[%s, %s]" s1 s2
   | Sqrt(s1, s2) -> sprintf "sqrt[%s, %s]" s1 s2
   | Add(s1, s2, s3) -> sprintf "add[%s, %s, %s]" s1 s2 s3
   | Sub(s1, s2, s3) -> sprintf "sub[%s, %s, %s]" s1 s2 s3
   | Mul(s1, s2, s3) -> sprintf "mul[%s, %s, %s]" s1 s2 s3
   | Div(s1, s2, s3) -> sprintf "div[%s, %s, %s]" s1 s2 s3
   | Ifgt(commands) -> sprintf "ifgt[ %s] " (String.concat ", " (List.map commandToString commands))
   | Ifeq(commands) -> sprintf "ifeq[ %s] " (String.concat ", " (List.map commandToString commands))
   | Ifge(commands) -> sprintf "ifeq[ %s] " (String.concat ", " (List.map commandToString commands))
   | Ifle(commands) -> sprintf "ifeq[ %s] " (String.concat ", " (List.map commandToString commands))
   | Iflt(commands) -> sprintf "ifeq[ %s] " (String.concat ", " (List.map commandToString commands))

let crnToString (ast: Crn) : string =
   let rec aux ast =
      match ast with
      | [] -> ""
      | Conc(s, c)::rest -> sprintf "conc[%s, %f], %s" s c (aux rest)
      | Step(commands)::rest ->
         let commandsString = String.concat ", " (List.map commandToString commands) in
         let restString = aux rest in
         if restString = "" then sprintf "step[ %s ]" commandsString
         else sprintf "step[%s], %s" commandsString (aux rest)
   in sprintf "crn = { %s }" (aux ast)

let parserCheck (ast:Crn) : bool =
   let astString = crnToString ast in
   let parsedAst = parse astString in
   parsedAst = ast
