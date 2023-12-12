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


let exampleInput2A = """
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
"""

[<Fact>]
let ``Answer 2 for example input 2A`` () =
  let input = parseInput exampleInput2A
  let actual = Result.bind answer2 input
  test
    <@ Ok 4 = actual @>

let exampleInput2B = """
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJIF7FJ-
L---JF-JLJIIIIFJLJJ7
|F|F-JF---7IIIL7L|7|
|FFJF7L7F-JF7IIL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
"""
[<Fact>]
let ``Answer 2 for example input 2B`` () =
  let input = parseInput exampleInput2B
  let actual = Result.bind answer2 input
  test
    <@ Ok 10 = actual @>
