namespace Checks

module ParserChecks =

   open Ast
   open Parser
   open FsCheck

   let rec commandToString (command: Command) : string =
      match command with
      | Load(S s1, S s2) -> sprintf "ld[%s, %s]" s1 s2
      | Cmp(S s1, S s2) -> sprintf "cmp[%s, %s]" s1 s2
      | Sqrt(S s1, S s2) -> sprintf "sqrt[%s, %s]" s1 s2
      | Add(S s1, S s2, S s3) -> sprintf "add[%s, %s, %s]" s1 s2 s3
      | Sub(S s1, S s2, S s3) -> sprintf "sub[%s, %s, %s]" s1 s2 s3
      | Mul(S s1, S s2, S s3) -> sprintf "mul[%s, %s, %s]" s1 s2 s3
      | Div(S s1, S s2, S s3) -> sprintf "div[%s, %s, %s]" s1 s2 s3
      | Ifgt(commands) -> sprintf "ifGT[ %s] " (String.concat ", " (List.map commandToString commands))
      | Ifeq(commands) -> sprintf "ifEQ[ %s] " (String.concat ", " (List.map commandToString commands))
      | Ifge(commands) -> sprintf "ifGE[ %s] " (String.concat ", " (List.map commandToString commands))
      | Ifle(commands) -> sprintf "ifLE[ %s] " (String.concat ", " (List.map commandToString commands))
      | Iflt(commands) -> sprintf "ifLT[ %s] " (String.concat ", " (List.map commandToString commands))
      | Rxn(_) -> failwith "Never generated"

   let crnToString (ast: Crn) : string =
      let rec aux ast =
         match ast with
         | [] -> ""
         | Conc(S s, c)::rest -> sprintf "conc[%s, %f], %s" s c (aux rest)
         | Step(commands)::rest ->
            let commandsString = String.concat ", " (List.map commandToString commands) in
            let restString = aux rest in
            if restString = "" then sprintf "step[ %s ]" commandsString
            else sprintf "step[%s], %s" commandsString (aux rest)
      in sprintf "crn = { %s }" (aux ast)


   let parserCheck (ast:Crn) : bool =
      let astString = crnToString ast in
      let parsedAst = parse astString in
      let check = parsedAst = ast
      if not check
      then printfn "check failed, parsed: \n%A" parsedAst; check
      else check

   let test (ast:Crn) : bool=
      printfn "AST:\n%A" ast
      printfn "stringified:\n%s" (crnToString ast)
      printfn "\n\n"
      true

   let runAll () =
      printfn $"Running Parser checks"
      let checkQuick lbl prop = Check.One ({Config.Quick with Name = lbl; EndSize = 20}, prop)
      Arb.register<AstGenerator.CustomGenerator>() |> ignore

      checkQuick "same as stringified ast" parserCheck

      printfn $"Parser checks done"
