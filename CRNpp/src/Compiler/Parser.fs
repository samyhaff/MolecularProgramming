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
      (fun s n -> Conc(s, n))

let ptwoargs (command: string) : Parser<Command> =
    skipString command >>. ws >>. skipChar '[' >>. ws
    >>. pipe2
      (pspecies .>> skipChar ',' .>> ws)
      (pspecies .>> ws .>> skipChar ']')
      (fun s1 s2 -> Load(s1, s2))

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
         | "add" -> Add(s1, s2, s3)
         | "sub" -> Sub(s1, s2, s3)
         | "mul" -> Mul(s1, s2, s3)
         | "div" -> Div(s1, s2, s3)
         | _ -> failwith "parithmetic: unknown operator")

let padd: Parser<Command> = pbinop "add"
let psub: Parser<Command> = pbinop "sub"
let pmul: Parser<Command> = pbinop "mul"
let pdiv: Parser<Command> = pbinop "div"

let psqrt: Parser<Command> =
  skipString "sqrt" >>. ws >>. skipChar '[' >>. ws
  >>. pipe2
    (pspecies .>> skipChar ',' .>> ws)
    (pspecies .>> ws .>> skipChar ']')
    (fun s1 s2 -> Sqrt(s1, s2))

let test p string =
   match run p string with
   | Success(result, _, _) -> printfn "%A" result
   | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
