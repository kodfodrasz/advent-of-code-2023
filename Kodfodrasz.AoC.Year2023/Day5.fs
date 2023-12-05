module Kodfodrasz.AoC.Year2023.Day5

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

type Seeds = int list

type AlmanacMapItem = 
  {
    Source: int
    Dest: int
    Length: int
  }
type AlmanacMap = AlmanacMapItem list

type Almanac = {
  Seeds : Seeds;
  SeedToSoilMap : AlmanacMap;
  SoilToFertilizerMap : AlmanacMap;
  FertilizerToWaterMap : AlmanacMap;
  WaterToLightMap : AlmanacMap;
  LightToTemperatureMap : AlmanacMap;
  TemperatureToHumidityMap : AlmanacMap;
  HumidityToLocationMap : AlmanacMap;
}

let parseSeeds line : Result<Seeds, string> =
  // EXAMPLE: seeds: 79 14 55 13
  Regex.Match(line, @"^seeds:\s+((?<seed>\d+)\s*)+$", RegexOptions.ExplicitCapture)
  |> function
  | m when m.Success -> 
      m.Groups["seed"].Captures 
      // the regexp mathced, so parse will always be successful!
      |> Seq.choose (fun c -> Parse.parseInt c.Value)
      |> Seq.toList 
      |> Ok
  | _ -> Error <| sprintf "Error parsing seeds.\n  line:\n%s" line


let parseBlock name (block:string) : Result<AlmanacMap, string>= 
  let error errmsg =
    Error <| sprintf "Error parsing %s map: %s\n  block:\n%s" name errmsg block

  // EXAMPLE:
  // seed-to-soil map:
  // 50 98 2
  // 52 50 48
  Regex.Match(
    block,
    @"(^(?<map>[-a-z]+) map:\s*$)(\n(^\s*(?<to>\d+)\s+(?<from>\d+)\s+(?<len>\d+)\s*$))+",
    RegexOptions.ExplicitCapture ||| RegexOptions.Multiline)
  |> function
  | m when m.Success -> 
      let mapName = m.Groups["map"].Value
      
      let items = 
        (m.Groups["to"].Captures, m.Groups["from"].Captures, m.Groups["len"].Captures)
        |||> Seq.map3 (fun t f l -> 
          (Parse.parseInt t.Value, Parse.parseInt f.Value, Parse.parseInt l.Value)
          |> function
            | Some ti, Some fi, Some li -> Ok { Dest = ti; Source = fi; Length = li}
            | _ -> error "integer parse error!")
        |> Seq.toList

      
      match mapName, items with
      | mapName, _ when mapName <> name -> error <| sprintf "unexpected map name encountered: %s" mapName
      | _, items -> 
        match List.tryFind Result.isError items with
        | Some (Error (e)) -> Error e // Just to adapt Result<int, string> -> Result<AlmanacMap,string>
        | _ -> items |> List.map Result.get |> Ok
  | _ -> error "format error"


let parseSeedtoSoilMap = parseBlock "seed-to-soil"
let parseSoilToFertilizerMap = parseBlock "soil-to-fertilizer"
let parseFertilizerToWaterMap = parseBlock "fertilizer-to-water"
let parseWaterToLightMap = parseBlock "water-to-light"
let parseLightToTemperatureMap = parseBlock "light-to-temperature"
let parseTemperatureToHumidityMap = parseBlock "temperature-to-humidity"
let parseHumidityToLocationMap = parseBlock "humidity-to-location"

let parseInput (input : string) : Result<Almanac,string> = 
  let takeLine = Seq.skipWhile String.isNullOrEmpty >> Seq.takeWhile String.notNullOrEmpty >> Seq.tryExactlyOne
  let takeBlock = Seq.skipWhile String.isNullOrEmpty >> Seq.takeWhile String.notNullOrEmpty >> String.join "\n"

  let consume (source : _ seq) = 
    let e = source.GetEnumerator()
    seq {
      while e.MoveNext() do
        yield e.Current
    }

  let consumable = 
    input.Split('\n')
    |> Seq.map String.trim
    |> Seq.skipWhile String.isNullOrEmpty
    |> consume

  // STRUCTURE:
  // seeds
  // seed-to-soil map
  // soil-to-fertilizer map
  // fertilizer-to-water map
  // water-to-light map
  // light-to-temperature map
  // temperature-to-humidity map
  // humidity-to-location map

  let seedsMaybe = 
    consumable 
    |> takeLine 
    |> Result.ofOption "expected seed line not found" 
    |> Result.bind parseSeeds

  // Railway orinted programming, you know! 
  let choochoo parser (seeds :Seeds, maps: AlmanacMap list) =
    consumable 
    |> takeBlock 
    |> parser 
    |> Result.map (fun smoke -> seeds, smoke :: maps)

  seedsMaybe
  |> function
  |  Ok seeds -> Ok(seeds, [])  // Start that train! 🚂
  |  Error e -> Error e         // Already failed :( 🚧
  |> Result.bind (choochoo parseSeedtoSoilMap)
  |> Result.bind (choochoo parseSoilToFertilizerMap)
  |> Result.bind (choochoo parseFertilizerToWaterMap)
  |> Result.bind (choochoo parseWaterToLightMap)
  |> Result.bind (choochoo parseLightToTemperatureMap)
  |> Result.bind (choochoo parseTemperatureToHumidityMap)
  |> Result.bind (choochoo parseHumidityToLocationMap)
  |> function
  |  Ok (seeds, [humidity2location; temperature2humidity; light2temperature; water2light; fertilizer2water; soil2fertilizer; seed2soil;]) ->
      Ok {
        Seeds = seeds;
        SeedToSoilMap = seed2soil;
        SoilToFertilizerMap = soil2fertilizer;
        FertilizerToWaterMap = fertilizer2water;
        WaterToLightMap = water2light;
        LightToTemperatureMap = light2temperature;
        TemperatureToHumidityMap = temperature2humidity;
        HumidityToLocationMap = humidity2location;
      }
  |  Error e -> Error e
  |  _ -> Error "Unexpected number of maps parsed."


let answer1 games =
  Error "TODO"

let answer2 games =
  Error "TODO"

type Solver() =
  inherit SolverBase("If You Give A Seed A Fertilizer")
  with
    override this.Solve input =
      input
      |>
      this.DoSolve
        parseInput 
        [ 
          answer1;
          answer2;
        ]
