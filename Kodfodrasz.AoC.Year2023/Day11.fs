module Kodfodrasz.AoC.Year2023.Day11
#nowarn "57" // Experimental language features (Array.Parallel)

let solverName = "Cosmic Expansion"

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC


let parseInputLine (input : string) : _ option = 
  Some <| input.ToCharArray()


let parseInput = 
  Parse.parsePuzzleInputLines parseInputLine
  >> Result.map array2D

let manhattan (x1, y1) (x2, y2) = 
  abs(x2-x1) + abs(y2-y1)

/// Array2d cheat sheet
///
/// > let a = Array2D.init 2 4 (fun row col -> 10 * row + col);;
/// val a: int array2d = [[0; 1; 2; 3]                              
///                      [10; 11; 12; 13]]                          
///                                                                 
/// > let row1 = a[1,*];;                                             
/// val row1: int array = [|10; 11; 12; 13|]                          
///                                                                 
/// > let col2 = a[*,2];;                                            
/// val col2: int array = [|2; 12|]                                  
///                                                                 

[<Literal>]
let SymbolSpace = '.'
[<Literal>]
let SymbolGalaxy = '#'

let expansion (data: char array2d) = 
  let emptyRows = 
    [0 .. Array2D.length1 data - 1]
    |> Seq.where (fun i -> 
        let row = data[i,*]
        row |> Seq.forall ((=)SymbolSpace))
    |> Seq.toArray
  let emptyColumns = 
    [0 .. Array2D.length1 data - 1]
    |> Seq.where (fun i -> 
        let row = data[*,i]
        row |> Seq.forall ((=)SymbolSpace))
    |> Seq.toArray
  (emptyRows, emptyColumns)

let galaxies (data: char array2d) =
  seq {
    for row in 0 .. Array2D.length1 data - 1 do
      for col in 0 .. Array2D.length2 data - 1 do
        if data[row,col] = SymbolGalaxy then yield (row,col)
  }

let expando (data : char array2d) (metric:int64) =
  let gals = data |> galaxies |> Seq.toArray
  let (emptyRows, emptyCols) = expansion data

  // TODO: this to lib?
  let pairs = 
    seq {
      for i in 0 .. Array.length gals - 2 do
        for j in i+1 .. Array.length gals - 1  do
          yield (gals[i], gals[j]);
    }
    |> Seq.toArray

  pairs
  |> Array.Parallel.map(fun (g1, g2) ->
        let dist = manhattan g1 g2 |> int64
        let extraR = 
          emptyRows 
          |> Array.where (fun r -> 
            let m = min (fst g1) (fst g2)
            let M = max (fst g1) (fst g2)
            // TODO: between
            m < r && r < M)
          |> Seq.length |> int64
        let extraC = 
          emptyCols 
          |> Seq.where (fun r -> 
            let m = min (snd g1) (snd g2)
            let M = max (snd g1) (snd g2)
            // TODO: between
            m < r && r < M)
          |> Seq.length |> int64
        dist + (metric - 1L) * (extraR + extraC))
  |> Array.Parallel.sum

let answer1 (data : char array2d) =
  expando data 2
  |> Ok

let answer2 (data) =
  expando data 1000000
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

