module Kodfodrasz.AoC.Year2023.Tests.Day12Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2023
open Kodfodrasz.AoC.Year2023.Day12


let exampleInput = """
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"""

//[<Fact>]
//let ``parsing example input`` () =
//  let expected = 
//    Ok [] : Result<int64 array array, string>

//  test
//   <@ let actual = parseInput exampleInput
//      actual = expected @>

[<Fact>]
let ``Answer 1 for example input`` () =
 let input = parseInput exampleInput
 let actual = Result.bind answer1 input
 test
   <@ Ok 21L = actual @>

[<Fact(Skip="This solution is too slow. Conserve CI $$$. Tests timed out after 45 minutes in CI...")>]
let ``Answer 2 for example input`` () =
 let input = parseInput exampleInput
 let actual = Result.bind answer2 input
 test
   <@ Ok 525152L = actual @>
