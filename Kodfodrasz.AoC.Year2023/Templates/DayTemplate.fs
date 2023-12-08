[<Kodfodrasz.AoC.AdventOfCode.IgnoreSolver("This is a template. Delete attribute when using")>]
module Kodfodrasz.AoC.Year2023.DayX

let solverName = "TODO"

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC


let parseInputLine (input : string) : _ option = 
  //Regex.Match(input, @"^\s*(?<cards>[2-9AKQJT]{5})\s+(?<bid>\d+)\s*$", RegexOptions.ExplicitCapture)
  //|> function
  //  | m when m.Success -> 
  //    let cards = m.Groups["cards"].Value.ToCharArray() |> Seq.choose Card.tryParse |> Seq.toList
  //    let bidMaybe = m.Groups["bid"].Value |> Parse.parseInt64
      
  //    match cards, bidMaybe with
  //    | cards, Some bid when 5 = List.length cards -> Some { Cards = cards; Bid = bid }
  //    | _ -> None
  //  | _ -> None
  failwith "TODO"


let parseInput : string -> Result<obj list,string> = 
  Parse.parsePuzzleInputLines parseInputLine

let answer1 (data : _ list) =
  failwith "TODO"

let answer2 (data : _ list) =
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
          answer1 >> note "THIS IS A TEMPLATE";
          answer2 >> note "THIS IS A TEMPLATE";
        ]

