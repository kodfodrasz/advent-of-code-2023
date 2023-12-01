module Kodfodrasz.AoC.Tests.SeqTest

open System
open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC

[<Fact>]
let ``Seq.batchBySeparator when input is empty seq`` () =
  let expected = List.empty<int list>

  let actual =
    Seq.empty<int>
    |> Seq.batchBySeparator 0
    |> Seq.toList

  test <@ expected = actual @>

[<Fact>]
let ``Seq.batchBySeparator when input is only separators`` () =
  let expected = List.empty<int list>

  let actual =
    [ 0; 0; 0; 0 ]
    |> Seq.ofList
    |> Seq.batchBySeparator 0
    |> Seq.toList

  test <@ expected = actual @>

[<Fact>]
let ``Seq.batchBySeparator when input has no separators`` () =
  let expected = [ [ 1; 2; 3; 4 ] ]

  let actual =
    [ 1; 2; 3; 4 ]
    |> Seq.ofList
    |> Seq.batchBySeparator 0
    |> Seq.toList

  test <@ expected = actual @>

[<Fact>]
let ``Seq.batchBySeparator when input has multiple batches separated with single separator`` () =
  let expected = [ [ 1; 2 ]; [ 3; 4 ]; [ 5; 6 ] ]

  let actual =
    [ 1; 2; 0; 3; 4; 0; 5; 6 ]
    |> Seq.ofList
    |> Seq.batchBySeparator 0
    |> Seq.toList

  test <@ expected = actual @>

[<Fact>]
let ``Seq.batchBySeparator when input has multiple batches separated some with multiple separators`` () =
  let expected = [ [ 1; 2 ]; [ 3; 4 ]; [ 5; 6 ]; [ 7 ] ]

  let actual =
    [ 1
      2
      0
      0
      3
      4
      0
      5
      6
      0
      0
      0
      0
      7 ]
    |> Seq.ofList
    |> Seq.batchBySeparator 0
    |> Seq.toList

  test <@ expected = actual @>
