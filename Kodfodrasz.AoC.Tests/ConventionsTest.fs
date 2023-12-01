module Kodfodrasz.AoC.Tests.ConventionsTest

open System
//open Microsoft.VisualStudio.TestTools.UnitTesting
open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC


module Year2010 =
  type Day8 = { NoISolver: bool }

  type Day43 =
    interface ISolver with
      member this.Year = failwith "TEST STUB"
      member this.Day = failwith "TEST STUB"
      member this.Name = failwith "TEST STUB"
      member this.Solve input = failwith "TEST STUB"

[<Fact>]
let ``Year Convention fails on not ISolver`` () =
  let yearMaybe =
    Conventions.solverYear typeof<Year2010.Day8>

  test <@ yearMaybe |> Result.isError @>

[<Fact>]
let ``Year Convention works on ISolver`` () =
  let yearMaybe =
    Conventions.solverYear typeof<Year2010.Day43>

  test <@ yearMaybe |> Result.get = 2010 @>

[<Fact>]
let ``Day Convention fails on not ISolver`` () =
  let dayMaybe =
    Conventions.solverDay typeof<Year2010.Day8>

  test <@ dayMaybe |> Result.isError @>

[<Fact>]
let ``Day Convention works on ISolver`` () =
  let dayMaybe =
    Conventions.solverDay typeof<Year2010.Day43>

  test <@ dayMaybe |> Result.get = 43 @>
