[<AutoOpen>]
[<RequireQualifiedAccess>]
module Kodfodrasz.AoC.Seq

let indexedFrom (start: int) series =
  let indices = Seq.initInfinite ((+) start)

  Seq.zip indices series

let startingFrom (start: int) = Seq.initInfinite ((+) start)

let batchBySeparator separator (sequence: seq<_>) =
  seq {
    let e = sequence.GetEnumerator()

    let mutable l = System.Collections.Generic.List<_>()

    while e.MoveNext() do
      if e.Current = separator then
        yield List.ofSeq l
        l.Clear()
      else
        l.Add e.Current

    yield List.ofSeq l
  }
  |> Seq.filter (List.isEmpty >> not)

let batchByPredicate predicate (sequence: seq<_>) =
  seq {
    let e = sequence.GetEnumerator()

    let mutable l = System.Collections.Generic.List<_>()

    while e.MoveNext() do
      if predicate e.Current then
        yield List.ofSeq l
        l.Clear()
      else
        l.Add e.Current

    yield List.ofSeq l
  }
  |> Seq.filter (List.isEmpty >> not)

let repeatInfinite (source : _ seq) = 
  seq {
    while true do
      yield! source
  }
