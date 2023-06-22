// Author: Samy Haffoudhi, 21/06/2023

namespace ChemicalEngine

module Parser =
   open FParsec
   open Reaction

   type Parser<'a> = Parser<'a, unit>

   let ws = spaces
   let token p = p .>> ws

   let pchemical: Parser<Name> =
      let charOrDigit c = isLetter c || isDigit c
      pstring "0" <|> token (many1Satisfy2L isLetter charOrDigit "species")

   let pspecies: Parser<Species> =
      pipe2 pchemical (ws >>. skipChar ':' >>. ws >>.pfloat) (fun s c -> s, c)

   let pspeciesList: Parser<Species list> = sepBy1 (pspecies .>> ws) (ws >>. pstring "," .>> ws)

   let pchemicals: Parser<Name list> =
      sepBy1 (pchemical .>> ws) (ws >>. pstring "+" .>> ws)
      |>> List.filter (fun s -> s <> "0")

   let prate: Parser<Rate> =
      (skipString "->" |>> fun _ -> 1.0)
      <|> (between (skipChar '('  >>. ws) (ws >>. skipChar ')') pfloat .>> ws .>> skipString "->")

   let convertToMultiplicity reaction =
      reaction
      |> List.groupBy fst
      |> List.map (fun (name, xs) -> (name, List.length xs))

   let preaction: Parser<Reaction> =
      pipe3
         pchemicals (ws >>. prate .>> ws) pchemicals
         (fun reactants rate products -> convertToMultiplicity <| List.map (fun s -> (s, 1)) reactants, rate, List.map (fun s -> (s, 1)) products)

   let preactions: Parser<Reaction list> = sepBy1 (preaction .>> ws) (ws >>. pstring "," .>> ws)

   let pConcentrationsAndReactions: Parser<Species list * Reaction list> =
      pipe2 (ws >>. pspeciesList .>> ws .>> skipChar ';') (ws >>. preactions .>> eof) (fun c r -> c, r)

   let test p string =
      match run p string with
      | Success(result, _, _) -> printfn "%A" result
      | Failure(errorMsg, _, _) -> printfn "Error: %s" errorMsg

   let parse string =
      match run pConcentrationsAndReactions string with
      | Success(result, _, _) -> result
      | Failure(errorMsg, _, _) -> failwithf "Error: %s" errorMsg
