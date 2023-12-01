module Kodfodrasz.AoC.Tests.Assert

open Xunit

open Kodfodrasz.AoC

//open Microsoft.VisualStudio.TestTools.UnitTesting

// Basic combinators

let equal<'T> (expected: 'T) (actual: 'T) = Assert.Equal<'T>(expected, actual)

let notEqual<'T> (notExpected: 'T) (actual: 'T) =
  Assert.NotEqual<'T>(notExpected, actual)

let isTrue expr = Assert.True expr
let isFalse expr = Assert.False expr

// Option helpers

let isSome maybe = Option.isSome maybe |> isTrue

let isSomeValue expected maybe =
  isSome maybe
  Option.get maybe |> equal expected

let isNone maybe = Option.isNone maybe |> isTrue

// Result helpers
let isOk maybe = Result.isOk maybe |> isTrue

let isOkValue expected maybe =
  isOk maybe
  Result.get maybe |> equal expected

let isError maybe = Result.isError maybe |> isTrue

let isErrorValue expectedError maybe =
  isError maybe

  match expectedError with
  | Error expected -> Result.getError maybe |> equal expected
  | unwrapped -> () //Result.getError maybe |> equal unwrapped
