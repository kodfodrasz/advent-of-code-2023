module Kodfodrasz.AoC.Year2023.Tests.Day11Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2023
open Kodfodrasz.AoC.Year2023.Day11


let exampleInput = """
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
"""

//[<Fact>]
//let ``parsing example input`` () =
//  let expected = 
//    Ok [|
//      [|0L; 3L; 6L; 9L; 12L; 15L;|];
//      [|1L; 3L; 6L; 10L; 15L; 21L;|];
//      [|10L; 13L; 16L; 21L; 30L; 45L;|]
//    |] : Result<int64 array array, string>

//  test
//   <@ let actual = parseInput exampleInput
//      actual = expected @>

[<Fact>]
let ``Answer 1 for example input`` () =
 let input = parseInput exampleInput
 let actual = Result.bind answer1 input
 test
   <@ Ok 374L = actual @>

[<Fact>]
let ``Answer 2 with lower metric 10 for example input`` () =
 let input = parseInput exampleInput
 let actual = Result.map (fun d -> expando d 10) input
 test
   <@ Ok 1030L = actual @>

[<Fact>]
let ``Answer 2 with lower metric 100 for example input`` () =
 let input = parseInput exampleInput
 let actual = Result.map (fun d -> expando d 100) input
 test
   <@ Ok 8410L = actual @>
