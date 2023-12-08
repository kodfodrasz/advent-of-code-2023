module Kodfodrasz.AoC.Year2023.Day8

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

type Direction = 
  | Left
  | Right

type Node = {
  Id : string
  Left : string
  Right : string
}

type DesertMap = {
  Directions : Direction list
  Crossroads : Node list
}

let parseInputDirectionsLine (input : string) : _ option = 
  Regex.Match(input, @"^\s*(?<move>[RL])*\s*$", RegexOptions.ExplicitCapture)
  |> function
    | m when m.Success -> 
      m.Groups["move"].Captures 
      |> Seq.map (fun c -> c.Value)
      |> Seq.map (function
          | "L" -> Left
          | "R" -> Right
          | _ -> failwith "regex match should have protected us!")
      |> Seq.toList
      |> Some
    | _ -> None

// nehéz szó
let parseÚtelágazódásokLine (input : string) : _ option = 
  Regex.Match(
    input, 
    @"^(?<node>(?<id>[A-Z]{3})\s*=\s*\(\s*(?<left>[A-Z]{3})\s*,\s*(?<right>[A-Z]{3})\s*\)\s*\n*)+$",
    RegexOptions.ExplicitCapture ||| RegexOptions.Multiline)
  |> function
    | m when m.Success -> 
      (m.Groups["id"].Captures 
      ,m.Groups["left"].Captures 
      ,m.Groups["right"].Captures )
      |||> Seq.map3 ( fun i l r -> { Id = i.Value; Left = l.Value; Right = r.Value } )
      |> Seq.toList
      |> Some
    | _ -> None

let parseInput (input : string) : Result<_,string> = 
  let consumable = 
    input.Split('\n')
    |> Seq.map String.trim
    |> Seq.skipWhile String.isNullOrEmpty
    |> Parse.consumeableSeq 
  
  let directionsMaybe = 
    consumable 
    |> Parse.takeLine 
    |> Option.bind parseInputDirectionsLine
    |> Result.ofOption "expected input directions line not found" 

  let mapMaybe =
    consumable 
    |> Parse.takeBlock 
    |> Option.bind parseÚtelágazódásokLine 
    |> Result.ofOption "expected directions block not found" 

  (directionsMaybe, mapMaybe)
  ||> Result.map2 (fun d m -> { Directions = d; Crossroads = m; })

let answer1 (data : DesertMap) =
  let directions = Seq.repeatInfinite data.Directions
  let start = "AAA"
  let goal = "ZZZ"

  let map = 
    data.Crossroads
    |> Seq.map (fun node -> (node.Id, node))
    |> Map.ofSeq

  let newLoc (loc:string) (dir:Direction) : string = 
    match loc, dir with
    | loc, Left -> map[loc].Left
    | loc, Right -> map[loc].Right

  seq {
    let mutable location = "AAA"
    for move in directions do
      yield location
      location <- newLoc location move
            
  }
  |> Seq.takeWhile ((<>) "ZZZ")
  |> Seq.length
  |> Ok

let answer2 (data : DesertMap) =
  failwith "TODO"

type Solver() =
  inherit SolverBase("Haunted Wasteland")
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

