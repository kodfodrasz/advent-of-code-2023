module Kodfodrasz.AoC.Year2023.Tests.Day1Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC.Year2023
open Kodfodrasz.AoC.Year2023.Day1


let exampleInput = """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
  """

[<Fact>]
let ``Parsing example input`` () =
  let expected: Result<int list list, string> = Ok [ 
    [
      1000
      2000
      3000
    ];
    [
      4000
    ];
    [
      5000
      6000
    ];
    [
      7000
      8000
      9000
    ];
    [
      10000
    ]
  ]

  test
    <@ let actual = parseInput exampleInput
       actual = expected @>

[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<int, string> = Ok 24000
       actual = expected @>

[<Fact>]
let ``Answer 2 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer2 input
       let expected: Result<int, string> = Ok 45000
       actual = expected @>
