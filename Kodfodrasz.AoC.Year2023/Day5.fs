module Kodfodrasz.AoC.Year2023.Day5

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

type Seeds = int64 list

type AlmanacMapItem = 
  {
    Source: int64
    Dest: int64
    Length: int64
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
      |> Seq.choose (fun c -> Parse.parseInt64 c.Value)
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
          (Parse.parseInt64 t.Value, Parse.parseInt64 f.Value, Parse.parseInt64 l.Value)
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


let lookup (map : AlmanacMap) num = 
  map 
  |> Seq.where(fun m -> m.Source <= num && num < m.Source + m.Length)
  |> Seq.sortByDescending(fun m -> m.Source)
  |> Seq.tryHead
  |> Option.map( fun mapping -> num - mapping.Source + mapping.Dest)
  |> Option.defaultValue num

let seed2location (almanac : Almanac) = 
  (lookup almanac.SeedToSoilMap)
  >> (lookup almanac.SoilToFertilizerMap)
  >> (lookup almanac.FertilizerToWaterMap)
  >> (lookup almanac.WaterToLightMap)
  >> (lookup almanac.LightToTemperatureMap)
  >> (lookup almanac.TemperatureToHumidityMap)
  >> (lookup almanac.HumidityToLocationMap)

let answer1 (almanac : Almanac) =
  almanac.Seeds
  |> List.map (seed2location almanac)
  |> List.min
  |> Ok

let answer2 (almanac : Almanac) =
  almanac.Seeds
  |> List.chunkBySize 2
  |> List.collect (function
    | [s; l] -> List.init ((int)l) ((int64)>>(+) s)
    | _ -> [])
  |> List.map (seed2location almanac)
  |> List.min
  |> Ok

type ValueRange = {
  Seed: Int64
  From  :  Int64
  Length:  Int64
}

// TODO: this is completely incorrect: 
//         1. non overlapping segments of ranges must be identity mapped
//         2. that is harder in case of multiple map overlaps with range... 
//       so this needs far more complex solution to work correctly.
let tryMapOverlapBefore (map:AlmanacMapItem) (valueRange : ValueRange) : ValueRange option =
  let doMap (mapping:AlmanacMapItem) num =
    num - mapping.Source + mapping.Dest

  let mS = map.Source
  let mE = map.Source + map.Length - 1L
  let sS = valueRange.From
  let sE = valueRange.From + valueRange.Length - 1L

  if mE < sS || sE < mS 
  then None
  else
    let s = max sS mS 
    let e = min sE mE
    Some { 
      Seed = valueRange.Seed; 
      From = (s |> doMap map); 
      Length = e - s + 1L }


let splitMap (map:AlmanacMap) values =
  values
  |> Seq.collect (fun v -> seq {
        for m in map do
          yield tryMapOverlapBefore m v
        done
    })
  |> Seq.choose id
  |> Seq.distinct
  |> Seq.toList

let answer2fast (almanac : Almanac) =
  let inputs = 
    almanac.Seeds
    |> List.chunkBySize 2
    |> Seq.choose (function
      |[s; l] -> Some { Seed = s; From = s; Length = l; } // I don't care about other cases now
      | _ -> None)
    |> Seq.toList

  let mapped = 
    inputs
    |> (splitMap almanac.SeedToSoilMap)
    |> (splitMap almanac.SoilToFertilizerMap)
    |> (splitMap almanac.FertilizerToWaterMap)
    |> (splitMap almanac.WaterToLightMap)
    |> (splitMap almanac.LightToTemperatureMap)
    |> (splitMap almanac.TemperatureToHumidityMap)
    |> (splitMap almanac.HumidityToLocationMap)
  
  let lowest = 
    mapped
    |> Seq.minBy(fun vr -> vr.From)

  Ok lowest.Seed
   
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
