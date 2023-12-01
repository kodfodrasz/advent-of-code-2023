module Kodfodrasz.AoC.Year2023.Day1

open System
open Kodfodrasz.AoC


let parseInput (input: string): Result<string list, string> =
  input.Split('\n')
  |> Seq.map String.trim
  |> Seq.skipWhile String.isNullOrEmpty
  // no transformation
  |> Seq.where String.notNullOrWhiteSpace
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

let answer1 (strings: string list) =
  let numbers = 
    strings
    |> Seq.map (fun (s:string) -> 
        s.ToCharArray() 
        |> Seq.where Char.IsDigit 
        |> Seq.map (string >> Int32.Parse)
        |> (fun i -> 10 * Seq.head i + Seq.last i))
    |> Seq.toArray

  numbers
  |> Seq.sum
  |> Ok

let substrings s = 
  let rec inner_substrings (acc:string list) s = 
      match s with
      | "" -> acc |> List.rev
      | _ -> inner_substrings (s :: acc) (s.Substring(1))

  inner_substrings [] s

let evilLigatureParser line =
  let parseStart = fun (s:string) ->
    match s with
    | s  when s.StartsWith "one" -> Some 1
    | s  when s.StartsWith "two" -> Some 2
    | s  when s.StartsWith "three" -> Some 3
    | s  when s.StartsWith "four" -> Some 4
    | s  when s.StartsWith "five" -> Some 5
    | s  when s.StartsWith "six" -> Some 6
    | s  when s.StartsWith "seven" -> Some 7
    | s  when s.StartsWith "eight" -> Some 8
    | s  when s.StartsWith "nine" -> Some 9
    | s  when (s.ToCharArray() |> Seq.tryHead |> Option.map Char.IsDigit |> Option.defaultValue false)
      -> s.Substring(0,1) |> Int32.Parse |> Some
    | _ -> None

  substrings line
  |> List.map parseStart
  |> List.choose id
  |> (fun i -> 10 * List.head i + List.last i)

let lol (line:string) =
  line
  // configing training ligature protection
    .Replace("one", "onee") // oneight -> oneeight, etc...
    .Replace("three", "three")
    .Replace("two", "twoo")
    .Replace("nine", "ninee")
    .Replace("eight", "eightt")
    // all to text, probably redundant
    //.Replace("1","one")
    //.Replace("2","two")
    //.Replace("3","three")
    //.Replace("4","four")
    //.Replace("5","five")
    //.Replace("6","six")
    //.Replace("7","seven")
    //.Replace("8","eight")
    //.Replace("9","nine")
    // back to digits
    .Replace("one","1")
    .Replace("two","2")
    .Replace("three","3")
    .Replace("four","4")
    .Replace("five","5")
    .Replace("six","6")
    .Replace("seven","7")
    .Replace("eight","8")
    .Replace("nine","9")
  |> (fun (s:string) -> 
           s.ToCharArray() 
           |> Seq.where Char.IsDigit 
           |> Seq.map (string >> Int32.Parse)
           |> (fun i -> 10 * Seq.head i + Seq.last i))

let answer2  (strings: string list) =
  //let differentParses = 
  //  strings
  //  |> Seq.map (fun l -> (l, evilLigatureParser l, lol l))
  //  |> Seq.where (fun (_,b,c) -> b <> c)
  //  |> Seq.toList

  let numbers = 
    strings
    |> Seq.map evilLigatureParser
  
  numbers
  |> Seq.sum
  |> Ok

type Solver() =
  inherit SolverBase("Trebuchet?!")
  with
    override this.Solve input =
      this.DoSolve parseInput [ answer1; answer2 ] input

