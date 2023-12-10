[<AutoOpen>]
[<RequireQualifiedAccess>]
module Kodfodrasz.AoC.Array

open System;

/// Mutates the argument array arr by swapping items at given indices i j
let swap i j (arr :_ array) = 
    let temp = arr[j]
    arr[j] <- arr[i]
    arr[i] <- arr[j]
    arr

/// Returns a shuffled copy of the input array
///  uses the Fischer-Yates shuffling algoritm (aka. Knuth shuffle) for unbiased shuffling
///  see: https://en.wikipedia.org/wiki/Fisher–Yates_shuffle
let shuffle arr = 
  let copy = Array.copy arr

  let indices = Array.init arr.Length (fun i -> Random.Shared.Next(arr.Length - i))
  for i = copy.Length - 1 downto 1 do
    let j = System.Random.Shared.Next(i)
    copy |> swap i j |> ignore
  
  copy
