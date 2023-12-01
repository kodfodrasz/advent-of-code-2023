[<AutoOpen>]
[<RequireQualifiedAccess>]
module Kodfodrasz.AoC.Result

let get result =
  match result with
  | Ok value -> value
  | Error e -> failwith e

let getError result =
  match result with
  | Ok _ -> failwith "Not an error!"
  | Error e -> e

let isOk result =
  match result with
  | Ok _ -> true
  | Error _ -> false

let isError result =
  match result with
  | Ok _ -> false
  | Error _ -> true

let ofOption errorValue option =
  match option with
  | Some value -> Ok value
  | None -> Error errorValue

let ofOptionWith errorThunk option =
  match option with
  | Some value -> Ok value
  | None -> Error(errorThunk ())

let defaultValue defval result =
  match result with
  | Ok value -> value
  | Error _ -> defval

let defaultWith (defThunk: 'E -> 'O) (result: Result<'O, 'E>) : 'O =
  match result with
  | Ok value -> value
  | Error e -> defThunk e

let sideEffect action result =
  match result with
  | Ok value -> action value
  | Error _ -> ()

  result

let map2 mapping first second =
  match first, second with
  | Ok firstVal, Ok secondVal -> Ok(mapping firstVal secondVal)
  | Ok _, Error err
  | Error err, Ok _
  | Error err, Error _ -> Error err
