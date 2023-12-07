module Kodfodrasz.AoC.Year2023.Tests.Day7Tests

open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC
open Kodfodrasz.AoC.Year2023
open Kodfodrasz.AoC.Year2023.Day7


let exampleInput = """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"""

//[<Fact>]
//let ``parsing example input`` () =
//  let expected = 
//    Ok [
//    // TODO
//    ]

//  test
//    <@ let actual = parseInput exampleInput
//       actual = expected @>

[<Fact>]
let ``Answer 1 for example input`` () =
  let input = parseInput exampleInput
  let actual = Result.bind answer1 input
  test
    <@ Ok 6440L = actual @>

[<Fact>]
let ``Answer 2 for example input`` () =
  let input = parseInput exampleInput
  let actual = Result.bind answer2 input
  test
    <@ Ok 5905L = actual @>

let parse2 (s : string) = 
  let cards = s.ToUpperInvariant().ToCharArray() |> Seq.choose Card2.tryParse |> Seq.toList
  assert (cards.Length = 5)
  cards

let permuteCards (s: string) =
  s.ToCharArray()
  |> Combinatorics.permutations
  |> Seq.map System.String

let evalHands2TestHelper isKind input = 
  input
  |> Seq.collect permuteCards
  |> Seq.map (fun s ->
      let c = parse2 s
      let y = evalHand2 c
      (isKind y, y))
  |> Seq.toList

[<Fact>]
let ``evalHands2 FiveOfAKind`` () =
  let inputs = [
    "KKKKK";
    "JKKKK";
    "JJKKK";
    "JJJKK";
    "JJJJK";
    "JJJJJ";
  ]

  let isFiveOfAKind = function
    | FiveOfAKind _ -> true
    | _ -> false

  let res = evalHands2TestHelper isFiveOfAKind inputs
  test
    <@ List.forall fst res @>

[<Fact>]
let ``evalHands2 FourOfAKind`` () =
  let inputs = [
    // no joker
    "KKKK2";
    // 1 joker
    "KKKJ2";
    "KKKJ2";
    "KKKJ2";
    // 2 jokers
    "KKJJ2";
    // 3 jokers
    "KJJJ2";
    // no 4 jokers case -> that makes the hand FiveOfAKind
  ]

  let isFourOfAKind = function
    | FourOfAKind _ -> true
    | _ -> false

  let res = evalHands2TestHelper isFourOfAKind inputs
  test
    <@ List.forall fst res @>

[<Fact>]
let ``evalHands2 FullHouse`` () =
  let inputs = [
    "KKAAA";
    // 1 joker
    "KKAAJ";
    // no 2 jokers case -> that makes the hand at least FourOfAKkind 
    // no 3 jokers -> that makes the hand at least FourOfAKkind 
    // no 4 jokers case -> that makes the hand FiveOfAKind
  ]

  let isFullHouse = function
    | FullHouse _ -> true
    | _ -> false

  let res = evalHands2TestHelper isFullHouse inputs
  test
    <@ List.forall fst res @>

[<Fact>]
let ``evalHands2 ThreeOfAKind`` () =
  let inputs = [
    "KKKAQ";
    // 1 joker
    "KQAAJ";
    // 2 jokers 
    "KQAJJ";
    // no 3 jokers -> that makes the hand at least FourOfAKkind
    // no 4 jokers case -> that makes the hand FiveOfAKind
  ]

  let isThreeOfAKind = function
    | ThreeOfAKind _ -> true
    | _ -> false

  let res = evalHands2TestHelper isThreeOfAKind inputs
  test
    <@ List.forall fst res @>

[<Fact>]
let ``evalHands2 TwoPair`` () =
  let inputs = [
    "KKQQA";
    // no joker -> that makes the hand at least ThreeOfAKind
  ]

  let isTwoPair = function
    | TwoPair _ -> true
    | _ -> false

  let res = evalHands2TestHelper isTwoPair inputs
  test
    <@ List.forall fst res @>

[<Fact>]
let ``evalHands2 OnePair`` () =
  let inputs = [
    "KKAQT";
    // 1 joker
    "KJAQT";
  ]

  let isOnePair = function
    | OnePair _ -> true
    | _ -> false

  let res = evalHands2TestHelper isOnePair inputs
  test
    <@ List.forall fst res @>

[<Fact>]
let ``evalHands2 HighCard`` () =
  let inputs = [
    "KQ234";
    // no joker -> that makes the hand at least OnePair
  ]

  let isHighCard = function
    | HighCard _ -> true
    | _ -> false

  let res = evalHands2TestHelper isHighCard inputs
  test
    <@ List.forall fst res @>
