module Kodfodrasz.AoC.Year2023.Day6

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

type Race = {
  Time: int
  WinningDistance : int
}

let parseInput (input : string) : Result<Race list,string> = 
  // EXAMPLE: Time:      7  15   30
  //        : Distance:  9  40  200
  Regex.Match(input, @"Time:\s+((?<time>\d+)\s*)+\nDistance:\s+((?<dist>\d+)\s*)+", RegexOptions.ExplicitCapture &&& RegexOptions.Multiline)
  |> function
  | m when m.Success -> 
      let times = 
        m.Groups["time"].Captures 
        // the regexp mathced, so parse will always be successful!
        |> Seq.choose (fun c -> Parse.parseInt c.Value)
      let dists = 
        m.Groups["dist"].Captures 
        // the regexp mathced, so parse will always be successful!
        |> Seq.choose (fun c -> Parse.parseInt c.Value)
      
      (times, dists)
      ||> Seq.zip 
      |> Seq.map (fun (t, d) -> {Time = t; WinningDistance = d;}) 
      |> Seq.toList 
      |> Ok
  | _ -> Error <| sprintf "Error parsing input:\n%s" input

///         Distance
///       ▲   ▲            xxx
///       │   |           xx▲xx
///       │   │         xxx │ xxWinDistance  │   │       xxx   │  xxx
///       │   │       x     │    xx
/// overY │   │      xx     │     xx
///       │   │     xx      │      xx    │
///       ▼   ├─────┬───────┼────────┬───┼─ Winning Distance
///           │   xx│       │        │   │
///           │   x │       │        │x  │
///           │  xx │       │        │xx │
///           │  x  │       │        │ x │
///           │ xx  │       │        │ x │
///           │ x   │       │        │  x│
///           │xx   │       │        │  x│
///           │x    │       │        │  x|
///         ──x─────┼───────┼────────┼───x──► Time
///           |     │       │        │   |
///                 │    Midpoint    │  Race Time
///                 │       |        │
///                 │overX1 |  overX2│
///                 ◄────   |    ────►
///                         | overXhalf
///                         |◄──────►|
let charge (race : Race) : int = 
  let isInteger (d:double) = Math.Floor d = d

  let midpointX = (double)race.Time / 2.0
  let midpointY = Math.Pow(midpointX, 2)
  let overY = midpointY - (double)race.WinningDistance
  let overXhalf = Math.Sqrt(overY)
  let overX1 = midpointX - overXhalf
  let overX2 = midpointX + overXhalf

  let mutable overX1int = (int)(Math.Ceiling(overX1))
  let mutable overX2int = (int)(Math.Floor(overX2))
  
  if (isInteger overX1) then overX1int <- overX1int + 1
  if (isInteger overX2) then overX2int <- overX2int - 1

  overX2int - overX1int + 1

let answer1 (races : Race list) =
  races
  |> List.map charge
  |> Seq.fold (*) 1
  |> Ok

let answer2 (races : Race list) =
  Error "TODO"

type Solver() =
  inherit SolverBase("Wait For It")
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

