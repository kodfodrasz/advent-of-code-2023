module Kodfodrasz.AoC.Year2023.Tests.Day7Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2023
open Kodfodrasz.AoC.Year2023.Day7


let exampleInput = """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
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

[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput
  let actual = Result.bind answer1 input
  test
    <@ Ok 6440L = actual @>


//[<Fact>]
//let ``Answer 2 for example input`` () =
//  let input = parseInput exampleInput
//  let actual = Result.bind answer2 input
//  test
//    <@ Ok 71503 = actual @>
