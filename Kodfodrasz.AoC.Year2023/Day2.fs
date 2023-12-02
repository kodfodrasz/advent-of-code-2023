module Kodfodrasz.AoC.Year2023.Day2

open System
open Kodfodrasz.AoC
open System.Text.RegularExpressions

type Handful = { Red: int; Green: int; Blue: int }
type Game = { Id: int; Pulls: Handful list }

let parseLine (line: string) =
  // Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
  let m = Regex.Match(line, "^Game (?<id>\d+): ((\s*(?<hands>(((?<b>\d+) blue)|((?<r>\d+) red)|((?<g>\d+) green)|\s+|,)+)\s*;?)+)")
  
  let getColor (m:Match) (hand:Capture) (color :string) = 
    let _start = hand.Index
    let _end = hand.Index + hand.Length
    
    m.Groups.[color].Captures 
      |> Seq.where (fun (capt : Capture) -> _start <= capt.Index && capt.Index <= _end)
      |> Seq.tryHead
      |> Option.bind (fun capt -> Parse.parseInt capt.Value)
      |> Option.defaultValue 0

  m
  |> function
  | m when m.Success ->
      let idMaybe = m.Groups.["id"].Value |> Parse.parseInt

      let hands = 
        m.Groups.["hands"].Captures 
        |> Seq.map( fun hg -> 
          let r = getColor m hg "r"
          let g = getColor m hg "g"
          let b = getColor m hg "b"
          { Red = r; Green = g; Blue = b; }
        )
        |> Seq.toList

      idMaybe 
      |> Option.map (fun id -> { Id=id; Pulls = hands})
  | _ -> None

let parseInput (input: string): Result<Game list, string> =
    //input.Split('\n')
    //|> Seq.map String.trim
    //|> Seq.where String.notNullOrWhiteSpace
    //|> Seq.map parseLine
    //|> Seq.toList
    //|> Ok
    Error "TODO"

let answer1 games =
  Error "TODO"

let answer2  games =
  Error "TODO"

type Solver() =
  inherit SolverBase("Cube Conundrum")
  with
    override this.Solve input =
      this.DoSolve parseInput [ answer1; answer2 ] input

