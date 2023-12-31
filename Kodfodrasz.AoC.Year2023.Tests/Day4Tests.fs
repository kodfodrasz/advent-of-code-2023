module Kodfodrasz.AoC.Year2023.Tests.Day4Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2023
open Kodfodrasz.AoC.Year2023.Day4


let exampleInput = """
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
  """

[<Fact>]
let ``parsing example input line`` () =
  let line = "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
  let expected= Some { 
    Id=3; 
    WinningNumbers = [1; 21; 53; 59; 44];
    Numbers = [69; 82; 63; 72; 16; 21; 14;  1];}

  test
    <@ let actual = parseLine line
       actual = expected @>


[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer1 input
       Ok 13 = actual @>


[<Fact>]
let ``Answer 2 for example input`` () =
  let input = parseInput exampleInput
       
  test
    <@ let actual = Result.bind answer2 input
      Ok 30 = actual @>

[<Fact>]
let ``Answer 2 FAST!!! for example input`` () =
  let input = parseInput exampleInput
       
  test
    <@ let actual = Result.bind answer2fast input
      Ok 30 = actual @>
