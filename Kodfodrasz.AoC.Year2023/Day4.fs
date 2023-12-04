module Kodfodrasz.AoC.Year2023.Day4

open System
open Kodfodrasz.AoC
open System.Text.RegularExpressions

type Game = {
  Id: int
  WinningNumbers: int list
  Numbers: int list
}

let parseLine (line: string) : Game option = 
  // EXAMPLE: Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
  // PUZZLE : Card   1: 73 92 13 35 18 96 37 72 76 39 | 82 14 66 57 25 98 49 28  3 95 81 85 31 30 16 79  7 12 55 19 97 45  9 58  2
  Regex.Match(line, @"Card\s+(?<id>\d+):(\s*(?<winning>\d+)\s+)+\|(\s*(?<having>\d+)\s*)+", RegexOptions.ExplicitCapture)
  |> function
  | m when m.Success ->
      let idMaybe = 
        m.Groups.["id"].Value |> Parse.parseInt
      let winNumsMaybe = 
        m.Groups.["winning"].Captures 
        |> Seq.choose (fun c -> c.Value |> Parse.parseInt) 
        |> Seq.toList
        |> function
          | l when 5 = List.length l || 10 = List.length l  -> Some l
          | _ -> None
      let numsMaybe = 
        m.Groups.["having"].Captures 
        |> Seq.choose (fun c -> c.Value |> Parse.parseInt) 
        |> Seq.toList
        |> function
          | l when 8 = List.length l || 25 = List.length l -> Some l
          | _ -> None
      idMaybe, winNumsMaybe, numsMaybe
    | _ -> None, None, None
  |||> Option.map3 (fun i w n -> {Id=i; WinningNumbers=w; Numbers=n;})

let parseInput (input: string): Result<Game list, string> =
    input.Split('\n')
    |> Seq.map String.trim
    |> Seq.where String.notNullOrWhiteSpace
    // TODO: Error Handling instead of Seq.choose!
    |> Seq.choose parseLine
    |> Seq.toList
    |> Ok

let gameValue (game : Game) = 
  let w = Set.ofList game.WinningNumbers
  let n = Set.ofList game.Numbers
  let intersect = Set.intersect w n
  if intersect.Count = 0 then
    0
  else 1 <<< (intersect.Count - 1)

let gameRewardCardIds (game : Game) = 
  let w = Set.ofList game.WinningNumbers
  let n = Set.ofList game.Numbers
  let intersect = Set.intersect w n

  Seq.init intersect.Count (fun i -> game.Id + i + 1)
  |> Seq.toList

let answer1 games =
  games
  |> Seq.sumBy gameValue
  |> sprintf "%i"
  |> Ok

let answer2 games =
  let lut = games |> Seq.map (fun g -> (g.Id, g)) |> Map.ofSeq

  let rec scratch = 
    function
    | ([],scratched) -> List.rev scratched
    | (card :: unscratched, scratched ) ->
      let game = Map.find card lut
      let rewards = gameRewardCardIds game
      if rewards |> List.isEmpty then scratch (unscratched, game :: scratched)
      else scratch (rewards @ unscratched, game :: scratched)

  let initial = lut.Keys |> Seq.sort |> Seq.toList

  let scratched = scratch (initial, [])

  List.length scratched 
  |> sprintf "%i (part 2 functional style)"
  |> Ok

let answer2fast games =
  let gameWinCount (game : Game) = 
    let w = Set.ofList game.WinningNumbers
    let n = Set.ofList game.Numbers
    let intersect = Set.intersect w n
    intersect.Count

  let wins = games |> Seq.map gameWinCount |> Seq.toArray
  let cards = Array.init (List.length games) (fun _ -> 1)

  for i in 0 .. cards.Length - 1 do
    if wins[i] <> 0 then
      for j in 1 .. wins[i] do
        let target = i + j
        if target < cards.Length then
          cards[target] <- cards[target] + cards[i]
      done
  done

  Array.sum cards
  |> sprintf "%i (part 2 imperative style)"
  |> Ok

type Solver() =
  inherit SolverBase("Scratchcards")
  with
    override this.Solve input =
      this.DoSolve (Parse.parsePuzzleInputLines parseLine) [ answer1; answer2fast; answer2 ] input

