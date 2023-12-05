module Kodfodrasz.AoC.Year2023.Tests.Day5Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2023
open Kodfodrasz.AoC.Year2023.Day5

[<Fact>]
let ``parsing example seeds line`` () =
  let line = "seeds: 79 14 55 13"
  let expected= Ok [79; 14; 55; 13;] : Result<Seeds,string>

  test
    <@ let actual = parseSeeds line
       actual = expected @>

[<Fact>]
let ``parsing example seed-to-soil map`` () =
  let block = 
    """seed-to-soil map:
    50 98 2
    52 50 48"""
  let expected =
    Ok [
      { Dest = 50; Source = 98; Length = 2};
      { Dest = 52; Source = 50; Length = 48};
    ] : Result<AlmanacMap,string>

  test
    <@ let actual = parseBlock "seed-to-soil" block
       actual = expected @>

let exampleInput = """
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"""

[<Fact>]
let ``parsing example input`` () =
  let expected = 
    Ok {
      Seeds = [79; 14; 55; 13;];
      SeedToSoilMap = [
        { Dest = 50; Source = 98; Length = 2};
        { Dest = 52; Source = 50; Length = 48};];
      SoilToFertilizerMap = [
        { Dest = 0; Source = 15 ; Length = 37; };
        { Dest = 37; Source = 52 ; Length = 2; };
        { Dest = 39; Source = 0 ; Length = 15; };];
      FertilizerToWaterMap = [
        { Dest = 49; Source = 53; Length = 8; };
        { Dest = 0; Source = 11; Length = 42; };
        { Dest = 42; Source = 0; Length = 7; };
        { Dest = 57; Source = 7; Length = 4; };];
      WaterToLightMap = [
        { Dest = 88; Source = 18; Length = 7; };
        { Dest = 18; Source = 25; Length = 70; };];
      LightToTemperatureMap = [
        { Dest = 45; Source = 77; Length = 23; };
        { Dest = 81; Source = 45; Length = 19; };
        { Dest = 68; Source = 64; Length = 13; };];
      TemperatureToHumidityMap = [
        { Dest = 0; Source = 69; Length = 1; };
        { Dest = 1; Source = 0; Length = 69; };];
      HumidityToLocationMap = [
        { Dest = 60; Source = 56; Length = 37; };
        { Dest = 56; Source = 93; Length = 4; };];
    } : Result<Almanac,string>

  test
    <@ let actual = parseInput exampleInput
       actual = expected @>

//[<Fact>]
//let ``Answer 1 for example input`` () =
//  let input = parseInput exampleInput

//  test
//    <@ let actual = Result.bind answer1 input
//       Ok 13 = actual @>


//[<Fact>]
//let ``Answer 2 for example input`` () =
//  let input = parseInput exampleInput
       
//  test
//    <@ let actual = Result.bind answer2 input
//      Ok 30 = actual @>
