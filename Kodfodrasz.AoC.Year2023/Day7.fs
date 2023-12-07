module Kodfodrasz.AoC.Year2023.Day7

open System
open System.Text.RegularExpressions
open Kodfodrasz.AoC

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

type Hand = 
    | HighCard of Card list
    | OnePair of Card list
    | TwoPair of Card list
    | ThreeOfAKind of Card list
    | FullHouse of Card list
    | FourOfAKind of Card list
    | FiveOfAKind of Card list

type CardsWithBid = {
  Cards : Card list
  Bid  : Int64 }

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

let evalHand (l : Card list)= 
  let allsame = 
    function 
    | [] -> true
    | head :: tail -> tail |> List.forall ((=) head)

  let allunique l = 
    (l |> Seq.distinct |> Seq.length) = (l |> Seq.length)

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

type Card2 =
 | Joker
 | Number of int
 | Tudjafene
 | Queen
 | King
 | Ace
  static member tryParse = 
    function
    | 'A' -> Some Ace
    | 'K' -> Some King
    | 'Q' -> Some Queen
    | 'J' -> Some Joker
    | 'T' -> Some Tudjafene
    | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as c -> Some <| Number (c |> string |> int)
    | _ -> None

let cardToCard2 (c:Card) : Card2 =
   match c with
   | Card.Number n -> Card2.Number n
   | Card.Tudjafene -> Card2.Tudjafene
   | Card.Jumbo -> Card2.Joker
   | Card.Queen -> Card2.Queen
   | Card.King -> Card2.King
   | Card.Ace -> Card2.Ace

type Hand2 = 
    | HighCard of Card2 list
    | OnePair of Card2 list
    | TwoPair of Card2 list
    | ThreeOfAKind of Card2 list
    | FullHouse of Card2 list
    | FourOfAKind of Card2 list
    | FiveOfAKind of Card2 list

type Cards2WithBid = {
  Cards : Card2 list
  Bid  : Int64 }

type Hand2WithBid = {
  Hand : Hand2
  Bid  : Int64 }

let evalHand2 (l : Card2 list)= 
  let allsameOrJoker l = 
    List.groupBy id l
    |> function 
    | [] -> true
    | [_] -> true
    | [ (k1,_); (k2,_); ] when k1 = Joker || k2 = Joker -> true
    | _ -> false

  List.sortDescending l // Joker is at list end!
  |> function
    | [ a; b; c; d; e ] when 
        allsameOrJoker [ a ; b; c; d; e] 
        -> FiveOfAKind l
    | [ a; b; c; d; e ] when 
        allsameOrJoker [ a ; b; c; d ] 
        || allsameOrJoker [ b; c; d; e ] 
        || allsameOrJoker [ a ; b; c; e ] // extra joker case
        || allsameOrJoker [ a ; b; d; e ] // extra joker case
        || allsameOrJoker [ a ; c; d; e ] // extra joker case
        -> FourOfAKind l 
    | [ a; b; c; d; e ] when 
        (allsameOrJoker [ a ; b; c ] && allsameOrJoker [ d; e ])
        || (allsameOrJoker [a ; b ] && allsameOrJoker [ c; d; e ]) 
        || (allsameOrJoker [a ; b; e] && allsameOrJoker [ c; d ]) // extra joker case
        -> FullHouse l 
    | [ a; b; c; d; e ] when 
        allsameOrJoker [ a ; b; c ] 
        || allsameOrJoker [ b ; c; d ] 
        || allsameOrJoker [ c; d; e]
        || (allsameOrJoker [ a; d; e]) // extra joker case
        || (allsameOrJoker [ a; b; e]) // extra joker case
        || (allsameOrJoker [ b; d; e]) // extra joker case
        || (allsameOrJoker [ b; c; e]) // extra joker case
        -> ThreeOfAKind l 
    | [ a; b; c; d; e ] when 
        (allsameOrJoker [ a ; b; ] && (allsameOrJoker [ c; d ] || allsameOrJoker [ d; e ] (* extra joker case -> *) || allsameOrJoker [ c; e ] ))
        || (allsameOrJoker [ b ; c; ] && allsameOrJoker [ d; e ])
        || (allsameOrJoker [ a ; e; ] && (allsameOrJoker [ b; c ] || allsameOrJoker [ c; d ] || allsameOrJoker [ b; d ])) // extra joker case
        -> TwoPair l 
    | [ a; b; c; d; e ] when 
        (allsameOrJoker [ a; b ] )
        || (allsameOrJoker [ b; c ]) 
        || (allsameOrJoker [ c; d ])
        || (allsameOrJoker [ d; e ])
        || (allsameOrJoker [ a; e ]) // extra joker case
        || (allsameOrJoker [ b; e ]) // extra joker case
        || (allsameOrJoker [ c; e ]) // extra joker case
        -> OnePair l 
    | [ _ ; _; _; _; _ ]  -> HighCard l 
    | _ -> failwith "Unexpected number of cards"

let answer2 (cards : CardsWithBid list) =
  cards
  |> Seq.map (fun cb -> { Cards = (List.map cardToCard2 cb.Cards); Bid = cb.Bid } )
  |> Seq.map (fun cb -> {
      Hand = evalHand2 cb.Cards;
      Bid = cb.Bid})
  |> Seq.sortBy (fun hb -> hb.Hand)
  |> Seq.toList
  |> Seq.mapi (fun i0 hb -> ((int64)i0 + 1L) * hb.Bid)
  |> Seq.sum
  |> Ok

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

