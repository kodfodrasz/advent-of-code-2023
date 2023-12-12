module Kodfodrasz.AoC.Year2023.Tests.Day10Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2023
open Kodfodrasz.AoC.Year2023.Day10


let exampleInput = """
..F7.
.FJ|.
SJ.L7
|F--J
LJ...
"""

let exampleInput2 = """
.....
.S-7.
.|.|.
.L-J.
.....
"""

[<Fact>]
let ``parsing example input`` () =
  let grid = array2D [
    [Ground; Ground; SouthEastPipeBend; SouthWestPipeBend; Ground]
    [Ground; SouthEastPipeBend; NorthWestPipeBend; VerticalPipe; Ground]
    [StartPosition(SouthEastPipeBend); NorthWestPipeBend;Ground; NorthEastPipeBend; SouthWestPipeBend]
    [VerticalPipe; SouthEastPipeBend; HorizontalPipe; HorizontalPipe; NorthWestPipeBend;]
    [NorthEastPipeBend; NorthWestPipeBend; Ground;Ground; Ground;]
  ]
  let expected : Result<PipeMap,string> = Ok { Grid = grid; StartPos = (0, 2)}

  test
   <@ let actual = parseInput exampleInput
      actual = expected @>

[<Fact>]
let ``Answer 1 for example input`` () =
 let input = parseInput exampleInput
 let actual = Result.bind answer1 input
 test
   <@ Ok 8 = actual @>

//[<Fact>]
//let ``Answer 2 for example input`` () =
// let input = parseInput exampleInput
// let actual = Result.bind answer2 input
// test
//   <@ Ok 2L = actual @>
