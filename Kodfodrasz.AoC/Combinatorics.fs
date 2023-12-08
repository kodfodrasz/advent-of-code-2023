[<AutoOpen>]
[<RequireQualifiedAccess>]
module Kodfodrasz.AoC.Combinatorics

/// Factorial
///  see: https://en.wikipedia.org/wiki/Factorial
let factorial n =
  let rec calc i acc =
    match i with
    | 0 | 1 -> acc
    | _ -> calc (i-1) (acc * i)
  calc n 1

/// Double Factorial (naive)
///  NOTE: not double floating point datatype!) 
///  see: https://en.wikipedia.org/wiki/Double_factorial
let doubleFactorial n =
  let rec calc i acc =
    match i with
    | 0 | 1 -> acc
    | _ -> calc (i-2) (acc * i)
  calc n 1

/// Binomial Coefficient (naive)
///  see: https://en.wikipedia.org/wiki/Binomial_coefficient
let binomialCoefficient n k =
  factorial n / (factorial n * factorial (n - k ))


/// Combinations
///  see: https://en.wikipedia.org/wiki/Combination
let combinationCount symbolCount draws = 
  binomialCoefficient symbolCount draws

/// Permutations
///  see https://en.wikipedia.org/wiki/Permutation
let permutationCount symbolCount = 
  binomialCoefficient symbolCount 1

/// Enumerate the permutations of the input array (Heap's algorithm)
///  see https://en.wikipedia.org/wiki/Heap%27s_algorithm
let permutations (array : _ array) = 
  let rec generate k (A : 'a array) : 'a array seq =
    seq {
      if k = 1 then
        yield Array.copy A
      else
        // Generate permutations with k-th unaltered
        // Initially k = length(A)
        yield! generate (k - 1) A

        // Generate permutations for k-th swapped with each k-1 initial
        for i = 0 to k - 2 do
          // Swap choice dependent on parity of k (even or odd)
          if k % 2 = 0 then
              // zero-indexed, the k-th is at k-1
              let temp = A.[i]
              A.[i] <- A.[k - 1]
              A.[k - 1] <- temp
          else
              // zero-indexed, the k-th is at k-1
              let temp = A.[0]
              A.[0] <- A.[k - 1]
              A.[k - 1] <- temp
          yield! generate (k - 1) A
        done
    }

  if Array.isEmpty array then Seq.empty<'a array>
  else 
    let scratch = Array.copy array
    if Array.length array = 1 then Seq.singleton scratch
    else generate (Array.length scratch) scratch
