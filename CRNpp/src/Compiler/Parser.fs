// Author: Samy Haffoudhi, 21/06/2023

module Parser

open FParsec
open Ast

type Parser<'a> = Parser<'a, unit>

let ws = spaces
let token p = p .>> ws

let pnumber: Parser<float> = token pfloat

let pspecies: Parser<string> =
   let charOrDigit c = isLetter c || isDigit c
   token (many1Satisfy2L isLetter charOrDigit "species")

let pconc: Parser<Root> =
    skipString "conc" >>. ws >>. skipChar '[' >>. ws
    >>. pipe2
      (pspecies .>> skipChar ',' .>> ws)
      (pnumber .>> ws .>> skipChar ']')
      (fun s n -> Conc(S s, n))

let pexpr: Parser<string list> =
   sepBy (pspecies .>> ws) (skipChar '+' >>. ws)

let prxn: Parser<Command> =
   skipString "rxn" >>. skipChar '[' >>. ws
   >>. pipe3
      (pexpr .>> skipChar ',' .>> ws)
      (pexpr .>> skipChar ',' .>> ws)
      (pnumber .>> ws .>> skipChar ']')
      (fun expr1 expr2 rate -> Rxn(List.map S expr1, List.map S expr2, rate))

let ptwoargs (command: string) : Parser<Command> =
    skipString command >>. ws >>. skipChar '[' >>. ws
    >>. pipe2
      (pspecies .>> skipChar ',' .>> ws)
      (pspecies .>> ws .>> skipChar ']')
      (fun s1 s2 ->
         match command with
         | "ld" -> Load(S s1, S s2)
         | "cmp" -> Cmp(S s1, S s2)
         | "sqrt" -> Sqrt(S s1, S s2)
         | _ -> failwith "ptwoargs: unknown command")

let pload: Parser<Command> = ptwoargs "ld"
let pcmp: Parser<Command> = ptwoargs "cmp"

let pbinop (op: string) : Parser<Command> =
    skipString op >>. ws >>. skipChar '[' >>. ws
    >>. pipe3
      (pspecies .>> skipChar ',' .>> ws)
      (pspecies .>> skipChar ',' .>> ws)
      (pspecies .>> ws .>> skipChar ']')
      (fun s1 s2 s3 ->
         match op with
         | "add" -> Add(S s1, S s2, S s3)
         | "sub" -> Sub(S s1, S s2, S s3)
         | "mul" -> Mul(S s1, S s2, S s3)
         | "div" -> Div(S s1, S s2, S s3)
         | _ -> failwith "parithmetic: unknown operator")

let padd: Parser<Command> = pbinop "add"
let psub: Parser<Command> = pbinop "sub"
let pmul: Parser<Command> = pbinop "mul"
let pdiv: Parser<Command> = pbinop "div"
let psqrt: Parser<Command> = ptwoargs "sqrt"

let (pifgt, pifgtimpl): Parser<Command> * Parser<Command> ref = createParserForwardedToRef ()
let (pifge, pifgeimpl): Parser<Command> * Parser<Command> ref = createParserForwardedToRef ()
let (piflt, pifltimpl): Parser<Command> * Parser<Command> ref = createParserForwardedToRef ()
let (pifle, pifleimpl): Parser<Command> * Parser<Command> ref = createParserForwardedToRef ()
let (pifeq, pifeqimpl): Parser<Command> * Parser<Command> ref = createParserForwardedToRef ()

let pcommand: Parser<Command> =
    choiceL [pload; prxn; padd; psub; pmul; pdiv; psqrt; pcmp; pifgt; pifge; piflt; pifle; pifeq] "command"

let pcommands: Parser<Command list> =
   sepBy (pcommand .>> ws) (skipChar ',' >>. ws)

pifgtimpl.Value <-
   skipString "ifGT" >>. ws >>. between (skipChar '[' >>. ws) (ws >>. skipChar ']') (pcommands |>> Ifgt)

pifgeimpl.Value <-
   skipString "ifGE" >>. ws >>. between (skipChar '[' >>. ws) (ws >>. skipChar ']') (pcommands |>> Ifge)

pifltimpl.Value <-
   skipString "ifLT" >>. ws >>. between (skipChar '[' >>. ws) (ws >>. skipChar ']') (pcommands |>> Iflt)

pifleimpl.Value <-
   skipString "ifLE" >>. ws >>. between (skipChar '[' >>. ws) (ws >>. skipChar ']') (pcommands |>> Ifle)

pifeqimpl.Value <-
   skipString "ifEQ" >>. ws >>. between (skipChar '[' >>. ws) (ws >>. skipChar ']') (pcommands |>> Ifeq)

let pstep: Parser<Root> =
    skipString "step" >>. ws >>. between (skipChar '[' >>. ws) (ws >>. skipChar ']') (pcommands |>> Step)

let proot: Parser<Root> = choiceL [pconc; pstep] "root"

let proots: Parser<Root list> =
   sepBy (proot .>> ws) (skipChar ',' >>. ws)

let pcrn: Parser<Crn> =
   skipString "crn" >>. ws >>. skipChar '=' >>. ws >>. between (skipChar '{' >>. ws) (ws >>. skipChar '}') proots .>> ws .>> eof

let parse string =
   match run pcrn string with
   | Success(result, _, _) -> result
   | Failure(errorMsg, _, _) -> failwith errorMsg

let test p string =
   match run p string with
   | Success(result, _, _) -> printfn "%A" result
   | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
