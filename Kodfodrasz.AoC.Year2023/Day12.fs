module Kodfodrasz.AoC.Year2023.Day12
#nowarn "57" // Experimental language features (Array.Parallel)

let solverName = "Hot Springs"

open System
open System.Text
open System.Text.RegularExpressions
open Kodfodrasz.AoC

type HotSpring = {
  Pattern : string
  DamagedGroups: int list
}

let parseDamage (s:string) = s.Split(",") |> Seq.choose Parse.parseInt |> Seq.toList
let formatDamage groups = groups |> Seq.map(sprintf "%i") |> String.concat ","
let countDamage (s:string) = 
  Regex.Match(s,@"([^#]*(?<s>[#]+)[^#]*)*", RegexOptions.ExplicitCapture)
  |> function
  |  m when m.Success -> 
        m.Groups["s"].Captures
        |> Seq.map(fun (c:Capture) -> c.Length)
        |> Seq.toList
  |  _ -> []
  
let parseInputLine (input : string) : HotSpring option = 
  input.Split([|' '|])
  |> function
  |  [|pattern; damages|] -> Some { Pattern = pattern; DamagedGroups = (parseDamage damages) }
  | _ -> None

let parseInput : string -> Result<HotSpring list,string> = 
  Parse.parsePuzzleInputLines parseInputLine

let variations pattern =
  let rec permute acc (pattern:string) = 
    let idx = pattern.IndexOf('?')
    if idx < 0 then pattern :: acc
    else
      let sb = StringBuilder pattern
      sb.[idx] <- '.'
      let a = sb.ToString()
      sb.[idx] <- '#'
      let b = sb.ToString()

      let acc = (permute acc a)
      permute acc b

  permute [] pattern

let arrangements pattern (damages : int list)= 
  let variations = variations pattern |> List.toArray
  variations
  |> Array.Parallel.filter(fun variant -> damages = countDamage variant)

let answer1 (data : HotSpring list) =
  data
  |> List.toArray
  |> Array.Parallel.sumBy (fun hs -> arrangements hs.Pattern hs.DamagedGroups |> Array.length |> int64)
  |> Ok

let answer2 (data : HotSpring list) =
  data
  |> Seq.map (fun hs -> 
      let pattern = Seq.replicate 5 hs.Pattern |> String.join "?"
      let damage = Seq.replicate 5 hs.DamagedGroups |> Seq.collect id |> Seq.toList
      (pattern, damage)
  ) 
  |> Seq.toArray
  |> Array.Parallel.sumBy (fun (pattern, damage) -> arrangements pattern damage |> Array.length |> int64)
  |> Ok

type Solver() =
  inherit SolverBase(solverName)
  with
    override this.Solve input =
      input
      |>
      this.DoSolve
        (parseInput)
        [ 
          answer1;
          answer2;
        ]

