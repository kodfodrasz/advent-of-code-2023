module Kodfodrasz.AoC.Tests.CombinatoricsTests


open System
//open Microsoft.VisualStudio.TestTools.UnitTesting
open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC

[<Fact>]
let ``Combinatorics.permutations empty input`` () =
  let input = [| |] : int array
  let expected = [] : int array list

  let permutations = 
    input 
    |> Combinatorics.permutations
    |> Seq.toList;

  test <@ expected = permutations @>

[<Fact>]
let ``Combinatorics.permutations singular input`` () =
  let input = [| 1 |]
  let expected = [
    [| 1 |]
  ]

  let permutations = 
    input 
    |> Combinatorics.permutations
    |> Seq.toList;
  test <@ expected = permutations @>

[<Fact>]
let ``Combinatorics.permutations 2 inputs`` () =
  let input = [| 1; 2 |]
  let expected = [
    [| 1; 2 |]
    [| 2; 1 |]
  ]

  let permutations = 
    input 
    |> Combinatorics.permutations
    |> Seq.toList;
  test <@ expected = permutations @>

[<Fact>]
let ``Combinatorics.permutations 3 inputs`` () =
  let input = [| 1; 2; 3 |]
  let expected = [
    [| 1; 2; 3 |] 
    [| 2; 1; 3 |] 
    [| 3; 1; 2 |] 
    [| 1; 3; 2 |] 
    [| 2; 3; 1 |] 
    [| 3; 2; 1 |] 
  ]

  let permutations = 
    input 
    |> Combinatorics.permutations
    |> Seq.toList;
  test <@ expected = permutations @>
