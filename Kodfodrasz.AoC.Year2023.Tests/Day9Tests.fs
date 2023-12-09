module Kodfodrasz.AoC.Year2023.Tests.Day9Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2023
open Kodfodrasz.AoC.Year2023.Day9


let exampleInput = """
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"""

[<Fact>]
let ``parsing example input`` () =
  let expected = 
    Ok [|
      [|0L; 3L; 6L; 9L; 12L; 15L;|];
      [|1L; 3L; 6L; 10L; 15L; 21L;|];
      [|10L; 13L; 16L; 21L; 30L; 45L;|]
    |] : Result<int64 array array, string>

  test
   <@ let actual = parseInput exampleInput
      actual = expected @>

[<Fact>]
let ``Answer 1 for example input`` () =
 let input = parseInput exampleInput
 let actual = Result.bind answer1 input
 test
   <@ Ok 114L = actual @>

//[<Fact>]
//let ``Answer 2 for example input`` () =
//  let input = parseInput exampleInput
//  let actual = Result.bind answer2 input
//  test
//    <@ Ok 5905L = actual @>
