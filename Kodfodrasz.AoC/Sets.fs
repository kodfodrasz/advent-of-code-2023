module Kodfodrasz.AoC.Sets

let descartes2 first second =
  seq {
    for f in first do
      for s in second do
        yield (f, s)
  }

let descartes3 first second third =
  seq {
    for f in first do
      for s in second do
        for t in third do
          yield (f, s, t)
  }
