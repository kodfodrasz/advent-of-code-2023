module Kodfodrasz.AoC.Year2023.Tests.Day1Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC.Year2023
open Kodfodrasz.AoC.Year2023.Day1


let exampleInput = """
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
  """

[<Fact>]
let ``Parsing example input`` () =
  let expected: Result<string list, string> = Ok [ 
    "1abc2";
    "pqr3stu8vwx";
    "a1b2c3d4e5f";
    "treb7uchet";
  ]

  test
    <@ let actual = parseInput exampleInput
       actual = expected @>

[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<int, string> = Ok 142
       actual = expected @>

[<Fact>]
let ``Helper for Answer 2`` () =
  let expected = [ 
    "abcd";
    "bcd";
    "cd";
    "d";
  ]

  test 
    <@
      let actual = Day1.substrings "abcd"
      actual = expected
    @>

let exampleInput2 = """
    two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen
    """

[<Fact>]
let ``Answer 2 for example input 2`` () =
  let input = parseInput exampleInput2

  test
    <@ let actual = Result.bind answer2 input
       let expected: Result<int, string> = Ok 281
       actual = expected @>
