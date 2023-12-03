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

type PreviousCell = 
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

let findNumbers (data : char array2d) =
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

  // Note: After consultation about the solution it became clear that the logic could be wastly simplified
  //       by extending the input with a '.' terminator column, which would cut most edgecases 
  //       in folder and in the retrieval of the possible last pending number at sequence end
  let folder row col (acc:PreviousCell) (c:char) = 
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

  Array2D.foldi data folder empty
  |> function
  | DigitSpan (rd, start, pns ) -> 
    // last cell was a digit, and it has not been pushed to the result list!
    let partnum = Seq.rev rd |> Seq.toArray |> charsToPartnum start
    partnum :: pns 
  | NotDigit(pns) -> pns
  |> Seq.rev

let answer1 (data : char array2d) =
  let numbers = 
    findNumbers data
    |> Seq.toArray

  let hasSymbolNeighbour = ((surroundingChars data (* n *)) >> (Seq.exists isSymbol)) // Seq.exists is the same as Enumerable.Any
  let realPartNumbers = 
    numbers
    |> Seq.where hasSymbolNeighbour

  realPartNumbers
  |> Seq.sumBy (fun pn -> pn.Value)
  |> Ok

let answer2 data =
  let numbers = 
    findNumbers data
    |> Seq.toArray
  
  numbers
  |> Seq.collect( fun n -> // this is a flatMap
      surroundingCells data n 
      |> Seq.choose (function // this is combination of where and map
          | (row, col, '*') -> Some (row, col, n)
          | _ -> None))
  // Group numbers neighbouring a symbol based on symbol indices
  |> Seq.groupBy(fun (r, c, n) -> (r,c))
  // filter the groups for those stars with exactly 2 neighbours
  |> Seq.where (fun (key, values) -> 2 = Seq.length values)
  // sum the product of the group values
  |> Seq.sumBy (fun (key, values) -> values |> Seq.map (fun (_,_,pn) -> pn.Value) |> Seq.fold (*) 1 )
  |> Ok

type Solver() =
  inherit SolverBase("Gear Ratios")
  with
    override this.Solve input =
      this.DoSolve parseInput [ answer1; answer2 ] input

