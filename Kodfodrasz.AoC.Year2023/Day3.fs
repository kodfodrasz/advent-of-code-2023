module Kodfodrasz.AoC.Year2023.Day3

open System
open Kodfodrasz.AoC

let parseInput (input: string): Result<char array2d, string> =
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
    |> Ok

type Index = { Row: int; Column: int; }
type PartNumber = {
  StartIndex: Index
  EndIndex: Index
  StringValue: string
  Value: int
}


let isDigit (c : char) = Char.IsDigit(c)
let isSymbol (c : char) = not(Char.IsDigit(c) || c = '.')

let getChar (arr: char array2d) (idx : Index) = 
  Array2D.get arr idx.Row idx.Column

type Accumulator1 = 
  | DigitSpan of revdigits : char list * start : Index * numbers: PartNumber list
  | NotDigit of PartNumber list

let surroundingCells (data : char array2d) (pn: PartNumber) = 
  seq {
    let rowMaxIdx = Array2D.length1 data - 1
    let colMaxIdx = Array2D.length2 data - 1
    
    // top Left
    if pn.StartIndex.Row > 0 && pn.StartIndex.Column > 0 then
      yield (pn.StartIndex.Row - 1 , pn.StartIndex.Column - 1)
    // top
    if pn.StartIndex.Row > 0 then
      for col in pn.StartIndex.Column .. pn.EndIndex.Column do
        yield (pn.StartIndex.Row - 1 , col)
    // top right
    if pn.StartIndex.Row > 0 && pn.EndIndex.Column < colMaxIdx then
      yield (pn.StartIndex.Row - 1 , pn.EndIndex.Column + 1)
    // right
    if pn.EndIndex.Column < colMaxIdx then
      yield (pn.StartIndex.Row, pn.EndIndex.Column + 1)
    // bottom right
    if pn.StartIndex.Row < rowMaxIdx && pn.EndIndex.Column < colMaxIdx then
      yield (pn.StartIndex.Row + 1 , pn.EndIndex.Column + 1)
    // bottom
    if pn.StartIndex.Row < rowMaxIdx then
      for col in pn.EndIndex.Column .. -1 .. pn.StartIndex.Column do
        yield (pn.StartIndex.Row + 1 , col)
    // bottom left
    if pn.StartIndex.Row < rowMaxIdx && pn.StartIndex.Column > 0 then
      yield (pn.StartIndex.Row + 1 , pn.StartIndex.Column - 1) 
    // left
    if pn.StartIndex.Column > 0 then
      yield (pn.StartIndex.Row, pn.StartIndex.Column - 1)
    }
  |> Seq.map (fun (row, col) -> 
      let char = Array2D.get data row col
      row, col, char)

let surroundingChars (data : char array2d) (pn: PartNumber) = 
  surroundingCells data pn 
  |> Seq.map (fun (row, col, char) -> char)

let answer1 (data : char array2d) =
  let empty = NotDigit([])

  let charsToPartnum startidx (chars : char array) = 
    let str = String(chars)
    let value = Parse.parseInt str |> Option.get // I know all items are digits!

    { 
      Value = value; 
      StringValue = str; 
      StartIndex = startidx; 
      EndIndex = { startidx with Column = (startidx.Column + chars.Length - 1) }
    }

  let folder row col (acc:Accumulator1) (c:char) = 
      match acc with
      | NotDigit (pns) when c |> isDigit -> DigitSpan ([c] , {Row=row; Column = col}, pns)
      | DigitSpan (rd, start, pns ) when (c |> isDigit) -> 
          if(start.Row = row) then
            // additional digit to number being parsed
            DigitSpan (c ::rd , start, pns)
          else
            let partnum = Seq.rev rd |> Seq.toArray |> charsToPartnum start
            DigitSpan ([c] , {Row=row; Column = col}, partnum :: pns)
      | DigitSpan (rd, start, pns ) when not(c |> isDigit) -> 
        let partnum = Seq.rev rd |> Seq.toArray |> charsToPartnum start
        NotDigit (partnum :: pns)
      | _ -> acc

  let numbers = 
    Array2D.foldi data folder empty
    |> function
    | DigitSpan (rd, start, pns ) -> 
      // last cell was a digit, and it has not been pushed to the result list!
      let row = Array2D.length1 data - 1
      let col = Array2D.length2 data - 1
      let partnum = Seq.rev rd |> Seq.toArray |> charsToPartnum start
      partnum :: pns 
    | NotDigit(pns) -> pns
    |> Seq.rev
    |> Seq.toArray

  let keep = 
    numbers
    |> Seq.where (fun n ->
      let surrounding = surroundingChars data n |> Seq.toArray
      let any = Seq.exists isSymbol surrounding
      any)
  
  keep
  |> Seq.sumBy (fun pn -> pn.Value)
  |> Ok

let answer2 data =
  // BEGIN COPY-PASTE
  let empty = NotDigit([])

  let charsToPartnum startidx (chars : char array) = 
    let str = String(chars)
    let value = Parse.parseInt str |> Option.get // I know all items are digits!

    { 
      Value = value; 
      StringValue = str; 
      StartIndex = startidx; 
      EndIndex = { startidx with Column = (startidx.Column + chars.Length - 1) }
    }

  let folder row col (acc:Accumulator1) (c:char) = 
      match acc with
      | NotDigit (pns) when c |> isDigit -> DigitSpan ([c] , {Row=row; Column = col}, pns)
      | DigitSpan (rd, start, pns ) when (c |> isDigit) -> 
          if(start.Row = row) then
            // additional digit to number being parsed
            DigitSpan (c ::rd , start, pns)
          else
            let partnum = Seq.rev rd |> Seq.toArray |> charsToPartnum start
            DigitSpan ([c] , {Row=row; Column = col}, partnum :: pns)
      | DigitSpan (rd, start, pns ) when not(c |> isDigit) -> 
        let partnum = Seq.rev rd |> Seq.toArray |> charsToPartnum start
        NotDigit (partnum :: pns)
      | _ -> acc

  let numbers = 
    Array2D.foldi data folder empty
    |> function
    | DigitSpan (rd, start, pns ) -> 
      // last cell was a digit, and it has not been pushed to the result list!
      let row = Array2D.length1 data - 1
      let col = Array2D.length2 data - 1
      let partnum = Seq.rev rd |> Seq.toArray |> charsToPartnum start
      partnum :: pns 
    | NotDigit(pns) -> pns
    |> Seq.rev
    |> Seq.toArray
  // END COPY-PASTE
  let map = Map.empty
  
  let groups = 
    numbers
    |> Seq.collect( fun n -> 
        surroundingCells data n 
        |> Seq.choose (function
            | (row, col, '*') -> Some (row, col, n)
            | _ -> None))
    |> Seq.groupBy(fun (r, c, n) -> (r,c))

  let pairs =
    groups
    |> Seq.where (fun (key, values) -> 2 = Seq.length values) 

  pairs
  |> Seq.sumBy (fun (key, values) -> values |> Seq.map (fun (_,_,pn) -> pn.Value) |> Seq.fold (*) 1 )
  |> Ok

type Solver() =
  inherit SolverBase("Gear Ratios")
  with
    override this.Solve input =
      this.DoSolve parseInput [ answer1; answer2 ] input

