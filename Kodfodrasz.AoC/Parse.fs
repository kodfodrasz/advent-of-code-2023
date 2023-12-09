module Kodfodrasz.AoC.Parse

open System
open System.Globalization


let tryParseWith (tryParseFunc: string -> bool * _) =
  tryParseFunc
  >> function
  | true, v -> Some v
  | false, _ -> None

// Integer types
let parseInt = tryParseWith Int32.TryParse
let parseUInt = tryParseWith UInt32.TryParse
let parseInt32 = tryParseWith Int32.TryParse
let parseUInt32 = tryParseWith UInt32.TryParse
let parseInt64 = tryParseWith Int64.TryParse
let parseUInt64 = tryParseWith UInt64.TryParse
// Floating point types
let parseSingle = tryParseWith Single.TryParse
let parseDouble = tryParseWith Double.TryParse
// Other types
let parseGuid = tryParseWith Guid.TryParse
let parseDate = tryParseWith DateTime.TryParse

let tryParseDay s =
  DateTime.TryParseExact(s, "yyyy-MM-dd", CultureInfo.InvariantCulture, DateTimeStyles.None)

let parseDay = tryParseWith tryParseDay

// Regex helpers
let captureValue (capture : System.Text.RegularExpressions.Capture) =
  capture.Value

// Puzzle inputs

let takeLine (strings: string seq) : string option = 
  strings 
  |> Seq.skipWhile String.isNullOrEmpty 
  |> Seq.takeWhile String.notNullOrEmpty 
  |> Seq.tryExactlyOne

let takeBlock (strings: string seq) : string option =
  let block = 
    strings
    |> Seq.skipWhile String.isNullOrEmpty
    |> Seq.takeWhile String.notNullOrEmpty 
    |> String.join "\n"
  if String.isNullOrWhiteSpace block then None
  else Some block

let consumeableSeq (source : _ seq) = 
  let e = source.GetEnumerator()
  seq {
    while e.MoveNext() do
      yield e.Current
  }

let parsePuzzleInputLines (parseLine : string -> _ option) (input: string) : Result<_ list, string> =
  let mapValidate parseLine (lines: string seq) = 
    let mutable errorLine = null
    use e = lines.GetEnumerator()
    let mutable state = []

    while (errorLine |> isNull) && e.MoveNext() do
        parseLine e.Current
        |> function
        | Some v -> state <- v :: state
        | None -> errorLine <- e.Current
    done

    if (errorLine |> isNull) then Ok (List.rev state)
    else Error (sprintf "Error parsing line: %s" errorLine)

  input.Split('\n')
  |> Seq.map String.trim
  |> Seq.skipWhile String.isNullOrEmpty
  |> Seq.where String.notNullOrWhiteSpace
  |> mapValidate parseLine
