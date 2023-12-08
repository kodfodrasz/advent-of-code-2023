[<AutoOpen>]
[<RequireQualifiedAccess>]
module Kodfodrasz.AoC.NumberTheory

/// Greatest Common Divisor
let inline gcd x y = 
  let mutable a = x
  let mutable b = y

  while b <> LanguagePrimitives.GenericZero do
    let tmp = a
    a <- b
    b <- tmp % b

  abs a

/// Least Common Multiple
let inline lcm x y = x * y / (gcd x y)
