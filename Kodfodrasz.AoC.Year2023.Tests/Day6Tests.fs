module Kodfodrasz.AoC.Year2023.Tests.Day6Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2023
open Kodfodrasz.AoC.Year2023.Day6


let exampleInput = """
Time:      7  15   30
Distance:  9  40  200
"""

[<Fact>]
let ``parsing example input`` () =
  let expected = 
    Ok [
      { Time = 7L; WinningDistance = 9L;}
      { Time = 15L; WinningDistance = 40L;}
      { Time = 30L; WinningDistance = 200L;}
    ] : Result<Race list,string>

  test
    <@ let actual = parseInput exampleInput
       actual = expected @>

[<Fact>]
let ``charge for first example`` () =
  test
    <@ 4 = charge { Time = 7L; WinningDistance = 9L;} @>

[<Fact>]
let ``charge for third example`` () =
  test
    <@ 9 = charge { Time = 30L; WinningDistance = 200L;} @>

[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput
  let actual = Result.bind answer1 input
  test
    <@ Ok 288 = actual @>


[<Fact>]
let ``Answer 2 for example input`` () =
  let input = parseInput exampleInput
  let actual = Result.bind answer2 input
  test
    <@ Ok 71503 = actual @>
