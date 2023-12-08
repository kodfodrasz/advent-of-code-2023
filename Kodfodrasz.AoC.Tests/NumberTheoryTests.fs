module Kodfodrasz.AoC.Tests.NumberTheoryTests


open System
//open Microsoft.VisualStudio.TestTools.UnitTesting
open Xunit
open Swensen.Unquote.Assertions

open Kodfodrasz.AoC

[<Fact>]
let ``NumberTheory.gcd tests`` () =
  test <@ 1 = NumberTheory.gcd 1 1 @>
  test <@ 1 = NumberTheory.gcd 7 15 @>
  test <@ 1 = NumberTheory.gcd 7 122 @>
  
  test <@ 3 = NumberTheory.gcd 9 6 @>
  test <@ 3 = NumberTheory.gcd 6 9 @>
  
[<Fact>]
let ``NumberTheory.lcm tests`` () =
  test <@ 1 = NumberTheory.lcm 1 1 @>
  test <@ 15 = NumberTheory.lcm 3 5 @>
  test <@ 3 = NumberTheory.lcm 3 3 @>
  
  test <@ 18 = NumberTheory.lcm 9 6 @>
  test <@ 18 = NumberTheory.lcm 6 9 @>
