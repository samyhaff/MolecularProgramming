// Date: 11/06/2023
// Contributor(s): Roar

module StringF

let split (delimiter:string) (source:string) : string list =
    source.Split(delimiter) |> Array.toList

let replace (pattern:string) (substitution:string) (source: string) :string =
    source.Replace (pattern, substitution)

let characters (source:string) :string list =
    source.ToCharArray() |> Array.map string |> Array.toList

let join (delimiter:string) (sources: string list) : string =
    System.String.Join(delimiter, (List.toArray sources))

let notWhitespace (source: string) :bool =
    System.String.IsNullOrWhiteSpace source |> not

let truncate (length:int) (source: string) :string =
    let stringLength = source.Length
    source.Substring(0, min length stringLength)
