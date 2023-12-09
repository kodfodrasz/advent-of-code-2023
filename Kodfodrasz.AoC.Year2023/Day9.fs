module Kodfodrasz.AoC.Year2023.Day9

let solverName = "Mirage Maintenance"

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

#nowarn "57" // Silence warning about experimental Array.Parallel features

let parseInputLine (input : string) : _ option = 
  let parts = input.Split([| ' '; '\t';|])
  
  parts
  |> Seq.map String.trim
  |> Seq.filter String.notNullOrWhiteSpace
  |> Seq.choose Parse.parseInt64
  |> Seq.toArray
  |> function
      | arr when not(Array.isEmpty arr) -> Some arr
      | _ -> None

let parseInput = 
  Parse.parsePuzzleInputLines parseInputLine 
  >> Result.map List.toArray

let inline allZero source = 
  if Seq.isEmpty source then true
  else Seq.forall ((=)LanguagePrimitives.GenericZero) source

let inline genDiff (source : _ array) = 
  let inline diffEntries (arr : _ array) = 
    Array.windowed 2 arr
    |> Array.Parallel.map (Array.reduce (fun a b -> b - a))
  
  seq {
    let mutable arr = Array.copy source
    while not(allZero arr) do
      yield arr
      arr <- diffEntries arr
    done
    yield arr
  }
  |> Seq.toArray

let answer1 (data : int64 array array) : Result<int64, string> =
  data
  |> Array.Parallel.map (genDiff >> Array.map Array.last >> Array.Parallel.sum)
  |> Array.Parallel.sum
  |> Ok

let answer2 data =
  failwith "TODO"

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

