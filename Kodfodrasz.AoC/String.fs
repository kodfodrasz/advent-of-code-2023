[<AutoOpen>]
[<RequireQualifiedAccess>]
module Kodfodrasz.AoC.String

let trim (s: string) = s.Trim()
let trimEnd (s: string) = s.TrimEnd()
let trimStart (s: string) = s.TrimStart()

let isNullOrEmpty = System.String.IsNullOrEmpty
let isNullOrWhiteSpace = System.String.IsNullOrWhiteSpace

let notNullOrEmpty = System.String.IsNullOrEmpty >> not
let notNullOrWhiteSpace = System.String.IsNullOrWhiteSpace >> not

let join (separator: string) (seq: string seq) = System.String.Join(separator, seq)

let split separator (str: string) = str.Split separator

let replace (a: string) (b: string) (str: string) = str.Replace(a, b)

/// Transliterate a set of characters to a different set much like the UNIX tr(1) tool
let tr (fromSymbols:string) (toSymbols:string) (input:string) =
  // TODO: ensure same length
  (fromSymbols.ToCharArray(), toSymbols.ToCharArray())
  ||> Array.zip
  |> Array.fold (fun (str:string) (a,b) -> str.Replace(a,b)) input
