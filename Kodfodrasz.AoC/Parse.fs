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
