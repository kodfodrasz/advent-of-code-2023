module Kodfodrasz.AoC.Year2023.Day7

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC
open System.Diagnostics

type Card =
 | Number of int
 | Tudjafene
 | Jumbo
 | Queen
 | King
 | Ace
  static member tryParse = 
    function
    | 'A' -> Some Ace
    | 'K' -> Some King
    | 'Q' -> Some Queen
    | 'J' -> Some Jumbo
    | 'T' -> Some Tudjafene
    | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as c -> Some <| Number (c |> string |> int)
    | _ -> None
  member this.getChar() = 
    match this with
    | Ace -> 'A'
    | King -> 'K'
    | Queen -> 'Q'
    | Jumbo -> 'J'
    | Tudjafene -> 'T'
    | Number n ->  int '0' + (int)n |> char

type Hand = 
    | HighCard of Card list
    | OnePair of Card list
    | TwoPair of Card list
    | ThreeOfAKind of Card list
    | FullHouse of Card list
    | FourOfAKind of Card list
    | FiveOfAKind of Card list

[<DebuggerDisplay("{DebuggerDisplay,nq}")>]
type CardsWithBid = {
  Cards : Card list
  Bid  : Int64 }
  with
    member this.DebuggerDisplay = 
      let cards = this.Cards |> Seq.map (fun c -> c.getChar |> string) |> String.join ""
      sprintf "%s %i" cards this.Bid

type HandWithBid = {
  Hand : Hand
  Bid  : Int64 }

let parseInputLine (input : string) : CardsWithBid option  = 
  Regex.Match(input, @"^\s*(?<cards>[2-9AKQJT]{5})\s+(?<bid>\d+)\s*$", RegexOptions.ExplicitCapture)
  |> function
    | m when m.Success -> 
      let cards = m.Groups["cards"].Value.ToCharArray() |> Seq.choose Card.tryParse |> Seq.toList
      let bidMaybe = m.Groups["bid"].Value |> Parse.parseInt64
      
      match cards, bidMaybe with
      | cards, Some bid when 5 = List.length cards -> Some { Cards = cards; Bid = bid }
      | _ -> None
    | _ -> None

let parseInput = Parse.parsePuzzleInputLines parseInputLine

let allsame = 
  function 
  | [] -> true
  | head :: tail -> tail |> List.forall ((=) head)
;;

let allunique l = 
  (l |> Seq.distinct |> Seq.length) = (l |> Seq.length)
;;

let evalHand (l : Card list)= 
  List.sort l
  |> function
    | [ a; b; c; d; e ] when 
        allsame [ a ; b; c; d; e] 
        -> FiveOfAKind l
    | [ a; b; c; d; e ] when 
        allsame [ a ; b; c; d ] 
        || allsame [ b; c; d; e ] 
        -> FourOfAKind l 
    | [ a; b; c; d; e ] when 
        (allsame [ a ; b; c ] && allsame [ d; e ])
        || (allsame [a ; b ] && allsame [ c; d; e ]) 
        -> FullHouse l 
    | [ a; b; c; d; e ] when 
        allsame [ a ; b; c ] 
        || allsame [ b ; c; d ] 
        || allsame [ c; d; e]
        -> ThreeOfAKind l 
    | [ a; b; c; d; e ] when 
        (allsame [ a ; b; ] && (allsame [ c; d ] || allsame [ d; e ]))
        || (allsame [ b ; c; ] && allsame [ d; e ])
        -> TwoPair l 
    | [ a; b; c; d; e ] when 
        (allsame [ a; b ] && allunique [ c; d; e ] )
        || (allsame [ b; c ] && allunique [ a; d; e ]) 
        || (allsame [ c; d ] && allunique [ a; b; e ])
        || (allsame [ d; e ] && allunique [ a; b; c ])
        -> OnePair l 
    | [ _ ; _; _; _; _ ]  -> HighCard l 
    | _ -> failwith "Unexpected number of cards"

let answer1 (cards : CardsWithBid list) =
  let orderedHands = 
    cards
    |> Seq.map (fun cb -> {
        Hand = evalHand cb.Cards;
        Bid = cb.Bid})
    |> Seq.sortBy (fun hb -> hb.Hand)
    |> Seq.toList

  orderedHands
  |> Seq.mapi (fun i0 hb -> ((int64)i0 + 1L) * hb.Bid)
  |> Seq.sum
  |> Ok

let answer2 (cards : CardsWithBid list) =
  failwith "TODO"

type Solver() =
  inherit SolverBase("Camel Cards")
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

