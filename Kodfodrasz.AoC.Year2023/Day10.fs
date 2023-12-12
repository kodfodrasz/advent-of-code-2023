module Kodfodrasz.AoC.Year2023.Day10

let solverName = "Pipe Maze"

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

// Map conventions used in the code:
///  │0
/// ─┼─────┬─► x
/// 0│..F7.│
///  │.FJ|.│
///  │SJ.L7│
///  │|F--J│
///  │LJ...│
///  ├─────┘
///  ▼
///  y
///
/// The containing Array2D contains the data continously scanned by rows,
/// thus it must be indexed as `Array2D.get y x arr` order


[<Literal>]
let SymbolVerticalPipe = '|'
[<Literal>]
let SymbolHorizontalPipe = '-'
[<Literal>]
let SymbolNorthEastPipeBend = 'L'
[<Literal>]
let SymbolNortWestPipeBend = 'J' 
[<Literal>]
let SymbolSouthWestPipeBend = '7'
[<Literal>]
let SymbolSouthEastPipeBend = 'F'
[<Literal>]
let SymbolGround = '.' 
[<Literal>]
let SymbolStartPosition = 'S'

type Cell = 
  | VerticalPipe 
  | HorizontalPipe 
  | NorthEastPipeBend 
  | NorthWestPipeBend 
  | SouthWestPipeBend 
  | SouthEastPipeBend 
  | Ground 
  | StartPosition of covered : Cell 

type Direction = North | East | South | West // Clockwise order! Important later!

let move (dir: Direction) (x, y) =
  match dir with
  | North -> (x, y-1)
  | East -> (x+1, y)
  | South -> (x, y+1)
  | West -> (x-1, y)

let opposite = function
| North -> South
| East -> West
| South -> North
| West -> East

let get arr (x, y) = 
  Array2D.get arr y x

let set arr (x, y) = 
  Array2D.set arr y x

let getSurroundings defval (x, y) (arr: _ array2d) = 
  // Indexing goes as arr[y,x] as data is contained horizontally continously

  // assume input array width/height >= 3
  let xoffs = 
    if x = 0 then x
    else x - 1
  let yoffs = 
    if y = 0 then y
    else y - 1

  let xtgt = if x = 0 then 1 else 0
  let ytgt = if y = 0 then 1 else 0

  let xlen = 
    if x = 0 || x = (Array2D.length2 arr - 1) then 2
    else 3
  let ylen = 
    if y = 0 || y = (Array2D.length1 arr - 1) then 2
    else 3

  let surrounding = Array2D.create 3 3 defval
  Array2D.blit arr yoffs xoffs surrounding ytgt xtgt ylen xlen
  surrounding

let rec getValidMoves = function
| VerticalPipe -> Some [| North; South |]
| HorizontalPipe -> Some [| West; East|]
| NorthEastPipeBend-> Some [| North; East |]
| NorthWestPipeBend -> Some [| North; West|]
| SouthWestPipeBend-> Some [| South; West|]
| SouthEastPipeBend-> Some [| South; East|]
| Ground -> None
| StartPosition (covered) -> getValidMoves covered

type PipeMap = {
  Grid : Cell array2d
  StartPos: (int * int)
}

let parseCells (input: char array2d) = 
  // First pass: just assign types to characters as J always confuses me
  let parseCellFirstPass = function
    | SymbolVerticalPipe-> Cell.VerticalPipe
    | SymbolHorizontalPipe-> Cell.HorizontalPipe
    | SymbolNorthEastPipeBend-> Cell.NorthEastPipeBend
    | SymbolNortWestPipeBend -> Cell.NorthWestPipeBend
    | SymbolSouthWestPipeBend-> Cell.SouthWestPipeBend
    | SymbolSouthEastPipeBend-> Cell.SouthEastPipeBend
    | SymbolStartPosition -> Cell.StartPosition(Ground)
    | SymbolGround | _ -> Cell.Ground

  let output = 
    input 
    |> Array2D.map parseCellFirstPass

  // Second pass: Determine the pipe part covered by StartPosition
  let mutable startX = 0;
  let mutable startY = 0;

  for y in 0 .. (Array2D.length1 output - 1) do
    for x in 0 .. (Array2D.length2 output - 1) do
      let pos = (x,y)
      let cell = output[y,x]
      match cell with
      | Cell.StartPosition _ -> 
        startX <- x
        startY <- y
        
        // 3 wide array2d with guaranteed content in every direction, centered on cell pos
        let kontext = getSurroundings Ground pos output

        let checkConnected dir = 
          // (1,1) is kontext center!
          move dir (1,1) |> get kontext |> getValidMoves 
          |> function
          | Some arr when arr |> Array.contains (opposite dir) -> Some dir
          | _ -> None

        let covered = 
          [North; East; South; West] 
          |> List.choose checkConnected 
          |> List.sort // member sorting based patterns!
          |> function
          | [North; South] -> VerticalPipe
          | [East; West] -> HorizontalPipe
          | [North; East] -> NorthEastPipeBend
          | [North; West] -> NorthWestPipeBend
          | [East; South] -> SouthEastPipeBend
          | [South; West] -> SouthWestPipeBend
          | _ -> Ground
        
        let start = Cell.StartPosition(covered)
        output[y,x] <- start
      | _ -> ()
    done
  done
  { Grid = output; StartPos=(startX, startY) }

let parseInput (input:string) : Result<PipeMap,string> = 
  let jaggedArray = 
    input.Split('\n')
    |> Seq.map String.trim
    |> Seq.where String.notNullOrWhiteSpace
    |> Seq.map (fun s -> s.ToCharArray())
    |> Seq.toArray
  
  // assume all rows have the same length
  let numRows = jaggedArray.Length
  let numCols = jaggedArray.[0].Length
  
  Array2D.init numRows numCols (fun i j -> jaggedArray.[i].[j])
  |> parseCells
  |> Ok

let pipeTrail map = 
  let rec walk traversed pos step =
    move step pos
    |> function
    | newpos when newpos = map.StartPos -> List.rev traversed
    | newpos -> 
            let newCell = get map.Grid newpos
            let nextStep = 
              getValidMoves newCell
              |> function
              | Some [| dir1; dir2 |] -> if opposite step <> dir1 then dir1 else dir2
              | _ -> failwith "Malformed data? Pipe broken?"
            walk (newpos :: traversed) newpos nextStep

  let mutable firstStep = 
    get map.Grid map.StartPos |> getValidMoves 
    |> function
    | Some [| dir1; _ |] -> dir1
    | _ -> failwith "Malformed data? Start position has no valid moves."

  walk [] map.StartPos firstStep

let answer1 (map: PipeMap) =
  let pipe = pipeTrail map

  let halfLength = (List.length pipe + 1)/2
  Ok halfLength

let answer2 (data) =
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

