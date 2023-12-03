[<AutoOpen>]
[<RequireQualifiedAccess>]
module Kodfodrasz.AoC.Array2D

let fold array folder state = 
  seq {
    for row in 0 .. Array2D.length1 array - 1 do
      for col in 0 .. Array2D.length2 array - 1 do
        yield array.[row, col]
  }
  |> Seq.fold folder state

let foldi array folder state = 
  seq {
    for row in 0 .. Array2D.length1 array - 1 do
      for col in 0 .. Array2D.length2 array - 1 do
        yield (row, col, array.[row, col])
   }
  |> Seq.fold (fun acc (row, col, e) -> folder row col acc e) state

