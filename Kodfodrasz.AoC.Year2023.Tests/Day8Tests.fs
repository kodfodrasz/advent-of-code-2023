module Kodfodrasz.AoC.Year2023.Tests.Day8Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2023
open Kodfodrasz.AoC.Year2023.Day8


let exampleInput = """
"""

//[<Fact>]
//let ``parsing example input`` () =
//  let expected = 
//    Ok [
//    // TODO
//    ]

//  test
//    <@ let actual = parseInput exampleInput
//       actual = expected @>

//[<Fact>]
//let ``Answer 1 for example input`` () =
//  let input = parseInput exampleInput
//  let actual = Result.bind answer1 input
//  test
//    <@ Ok 6440L = actual @>

//[<Fact>]
//let ``Answer 2 for example input`` () =
//  let input = parseInput exampleInput
//  let actual = Result.bind answer2 input
//  test
//    <@ Ok 5905L = actual @>

[<Fact>]
let ``findRecurrence empty sequence`` () =
  test
    <@ None = Day8.findRecurrence [] @>

[<Fact>]
let ``findRecurrence singular sequence`` () =
  test
    <@ None = Day8.findRecurrence [ 1; ] @>

[<Fact>]
let ``findRecurrence recurring pattern short sequence`` () =
  test
    <@ Some (0, 1, 'a') = Day8.findRecurrence [ 'a'; 'a'; ] @>

[<Fact>]
let ``findRecurrence recurring pattern sequence`` () =
  test
    <@ Some (2, 5, 'c') = Day8.findRecurrence [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'c'; 'z'; 'x' ] @>
