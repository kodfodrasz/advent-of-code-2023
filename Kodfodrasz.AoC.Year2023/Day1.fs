module Kodfodrasz.AoC.Year2023.Day1

open Kodfodrasz.AoC


let parseInput (input: string): Result<int list list, string> =
  input.Split('\n')
  |> Seq.map String.trim
  |> Seq.skipWhile String.isNullOrEmpty
  |> Seq.batchBySeparator ""
  |> Seq.map (List.map (fun line -> Parse.parseInt line))
  |> Seq.map (List.choose id)
  |> Seq.toList
  |> Ok


  //let errorMaybe =
  //  Seq.tryFind (fun (_, numMaybe) -> Option.isNone numMaybe) numbersMaybe

  //match errorMaybe with
  //| None ->
  //    numbersMaybe
  //    |> Seq.map (fun (_, num) -> Option.get num)
  //    |> Seq.toList
  //    |> Ok
  //| Some (line, _) ->
  //    sprintf "Input line could not be parsed to integer: %s" line
  //    |> Error

let answer1 (numbers : int list list) =
  numbers
  |> List.map List.sum
  |> List.max
  |> Ok

let answer2 numbers =
  numbers
  |> List.map List.sum
  |> Seq.sortDescending
  |> Seq.take 3
  |> Seq.sum
  |> Ok

type Solver() =
  inherit SolverBase("Calorie Counting")
  with
    override this.Solve input =
      this.DoSolve parseInput [ answer1; answer2 ] input
