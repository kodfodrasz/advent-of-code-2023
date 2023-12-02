module Kodfodrasz.AoC.Year2023.Tests.Day2Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC.Year2023
open Kodfodrasz.AoC.Year2023.Day2


[<Fact>]
let ``Parsing example line 1`` () =
  let res = parseLine "Game 1: 2 blue, 3 green; 4 green, 5 blue, 6 red; 7 green, 8 blue"
  let expected = Some {
    Id = 1;
    Pulls = 
    [
      { Red = 0; Green = 3; Blue = 2};
      { Red = 6; Green = 4; Blue = 5};
      { Red = 0; Green = 7; Blue = 8};
    ]
  }

  test
    <@ expected = res @>


