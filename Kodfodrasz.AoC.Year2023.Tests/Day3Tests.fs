module Kodfodrasz.AoC.Year2023.Tests.Day3Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2023
open Kodfodrasz.AoC.Year2023.Day3


let exampleInput = """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
  """

//[<Fact>]
//let ``Parsing example input`` () =
//  let expected: Result<string list, string> = Error "TODO"

//  test
//    <@ let actual = parseInput exampleInput
//       actual = expected @>

[<Fact>]
let ``surroundingCells test`` () =
  let jaggedArray = [
    ['1'; '2'; '3'; '4'];
    ['5'; '6'; '7'; '8'];
    ['9'; 'a'; 'b'; 'c'];
  ]
  let numRows = jaggedArray.Length
  let numCols = jaggedArray.[0].Length

  let data = Array2D.init numRows numCols (fun i j -> jaggedArray.[i].[j])

  test
    <@ 
      "12348cba95".ToCharArray() = (Day3.surroundingChars data {
      StartIndex = { Row = 1; Column = 1;};
      EndIndex = { Row = 1; Column = 2;};
      StringValue = ""; Value = 0 } |> Seq.toArray)
    @>

[<Fact>]
let ``surroundingCells test 2`` () =
  let data = parseInput exampleInput |> Result.get

  let expected = 
    "....*....".ToCharArray();
  let actual =
    Day3.surroundingChars data {
      StartIndex = { Row = 4; Column = 0;};
      EndIndex = { Row = 4; Column = 2;};
      StringValue = "617"; Value = 617 } 
    |> Seq.toArray

  test
    <@ expected = actual @>

[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput

  test
    <@ let actual = Result.bind answer1 input
       let expected: Result<int, string> = Ok 4361
       actual = expected @>
