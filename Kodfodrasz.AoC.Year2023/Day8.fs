module Kodfodrasz.AoC.Year2023.Day8


#nowarn "57" // Experimental language features (Array.Parallel)

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

let newLoc (map:Map<string, Node>) (loc:string) (dir:Direction) : string = 
  match loc, dir with
  | loc, Left -> map[loc].Left
  | loc, Right -> map[loc].Right

let answer1 (data : DesertMap) =
  let directions = Seq.repeatInfinite data.Directions
  let start = "AAA"
  let goal = "ZZZ"

  let map = 
    data.Crossroads
    |> Seq.map (fun node -> (node.Id, node))
    |> Map.ofSeq

  seq {
    let mutable location = start
    for move in directions do
      yield location
      location <- newLoc map location move
            
  }
  |> Seq.takeWhile ((<>) goal)
  |> Seq.length
  |> (int64)
  |> Ok

let findRecurrence (source : 'a seq) = 
  let mutable occured : Map<'a,int> = Map.empty
  let mutable index = 0

  seq {
    for sym in source do
      let phase = (Map.tryFind sym occured |> Option.defaultValue -1)

      // phase marks the first occirence of the symbol
      if(0 <= phase )then
        let period = index - phase
        yield Some (phase, period, sym)
      else 
        occured <- Map.add sym index occured
        yield None
      index <- index + 1
  }
  |> Seq.skipWhile Option.isNone
  |> Seq.tryHead 
  |> Option.flatten

/// see: https://chat.openai.com/share/e86ec0c1-aa86-4cdd-abf3-fb08275e4a09
let answer2 (data : DesertMap) =
  let directions = Seq.repeatInfinite data.Directions
  let start = 
    data.Crossroads
    |> Seq.where (fun node -> node.Id.EndsWith "A")
    |> Seq.map (fun node -> node.Id)
    |> Seq.toArray
  let goal = 
    data.Crossroads
    |> Seq.where (fun node -> node.Id.EndsWith "Z")
    |> Seq.map (fun node -> node.Id)
    |> Set.ofSeq
  
  let map = 
    data.Crossroads
    |> Seq.map (fun node -> (node.Id, node))
    |> Map.ofSeq

  let walk start = 
    seq {
      let mutable location = start
      for move in directions do
        yield (location, move)
        location <- newLoc map location move
    }

  let isGoal (s:string) = s.EndsWith('Z')

  let experiment = 
    start 
    |> Array.map (walk)
    |> Array.Parallel.map (
      Seq.mapi (fun i j -> (i,j))
      >> Seq.filter (snd >> fst >> isGoal)
      >> Seq.take 10
      >> Seq.toArray)

  let pairs = 
    experiment 
    |> Array.Parallel.map(
      // take the index >> make pairs of neighbours >> map the pairs (arrays) to their differences
      Array.map fst >> Array.windowed 2)
  let diffs = 
    pairs
    |> Array.Parallel.map (Array.map (Array.reduce (fun a b -> b - a)))


  let isSingulars = 
    diffs 
    |> Array.Parallel.map (fun arr -> Array.forall ((=) (arr[0])) arr)

  if not (Array.forall id isSingulars) then failwith "expected uniform periods"


  let phases = experiment |> Array.Parallel.map(Array.head >> fst >> int64)
  let periods = diffs |> Array.Parallel.map(Array.head >> int64)
  let harmonics = Array.zip phases periods
  
  let zerobased = Array.map2 ((=)) phases periods |> Array.forall id
  if not zerobased then failwith "expected periods to be the same length as phases"

  periods 
  |> Array.Parallel.reduce NumberTheory.lcm
  |> Ok

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

