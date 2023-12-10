// fsharplint:disable NonPublicValuesNames
module Kodfodrasz.AoC.Cli.Program

open Kodfodrasz.AoC

open System
open System.IO
open System.Reflection
open Autofac
open BlackFox.ColoredPrintf

let AocYear = 2023

let PrintBanner year =
  colorprintf """
 $green[             __                 __           ____                  __   ]
 $green[  ____ _____/ /   _____  ____  / /_   ____  / __/  _________  ____/ /__ ] $red[%i]
 $green[ / __ `/ __  / | / / _ \/ __ \/ __/  / __ \/ /_   / ___/ __ \/ __  / _ \]
 $green[/ /_/ / /_/ /| |/ /  __/ / / / /_   / /_/ / __/  / /__/ /_/ / /_/ /  __/]
 $green[\__,_/\__,_/ |___/\___/_/ /_/\__/   \____/_/     \___/\____/\__,_/\___/ ]

                                          $yellow[Copyright © %i Kódfodrász]


  $darkgreen[Advent of Code | https://adventofcode.com/]

"""
    year year


module UserMenu =
  // MENU LISTING

  let ListSolvers (solvers: ISolver list) =
    let lastMaybe = List.tryLast solvers

    printfn ""
    printfn " # Available solvers"
    printfn ""

    for (i, s) in List.indexed solvers do
      let idx = i + 1
      let last = lastMaybe |> Option.get

      if s = last
      then printfn " >%2i< %4i Day %2i : %s" idx s.Year s.Day s.Name
      else printfn "  %2i  %4i Day %2i : %s" idx s.Year s.Day s.Name

  // MENU INPUT PARSING

  let (|Integer|_|) (input: string) =
    input.ToLower().Trim() |> Parse.parseInt

  type Command =
    | All
    | Last
    | Number of i: int
    | Exit
    | Invalid

  let parseCommand (input: string) =
    (input |> Option.ofObj |> Option.defaultValue "quit").Trim().ToLower()
    |> function
    | ""
    | "last" -> Last
    | "all" -> All
    | Integer i -> Number i
    | "q"
    | "quit"
    | "exit" -> Exit
    | _ -> Invalid

  let userPrompt (): string =
    printfn ""
    printfn " Select solver(s) to run by entering its number, or 'last', or 'all', then press enter!"
    printfn " To quit the program enter 'q', 'quit', or 'exit', then press enter!"
    printfn " If you don't select any command or solver, the 'last' command is assumed."
    printf "   solver [last]: "

    let input = Console.ReadLine()
    printfn ""
    input

  let getSolvers allSolvers (command: Command) =
    let lastMaybe = List.tryLast allSolvers

    match command with
    | All -> Some allSolvers
    | Last -> lastMaybe |> Option.map List.singleton
    | Number n ->
        allSolvers
        |> List.tryItem (n - 1)
        |> Option.map List.singleton
    | Exit -> Some []
    | _ ->
        printfn "    Could not find matching solver(s)! Please try again!"
        None

  let invalidInput () =
    printfn "    Could not find matching solver(s)! Please try again!"

  // SOLVER EXECUTION


  type SolverAnswer = Result<Solution, string>
  type SolverOutput = SolverAnswer list

  let formatSolverAnswer =
    function
    | Ok solution ->
        sprintf
          "  >> answer %i (%i ms) = %s"
          solution.Part
          ((int) solution.CalculationTime.TotalMilliseconds)
          solution.Value
    | Error err -> sprintf "  !! ERROR: %s" err

  let Solve options (solvers: ISolver list) =
    for solver in solvers do
      let puzzle = Puzzle.ofSolver solver

      printfn " ### %i day %i: %s " puzzle.Year puzzle.Day puzzle.Name
      printfn ""

      let input = Input.getInput options puzzle

      let output =
        input
        |> Result.map solver.Solve
        |> Result.defaultWith (Error >> Seq.singleton)

      for o in output do
        formatSolverAnswer o |> printfn "%s"

      printfn ""

  // GOOD BYE

  let Goodbye () =
    printfn ""
    printfn " << Good bye!"
    printfn ""

  let GoodbyeError error =
    printfn ""
    printfn " <! Error occured! Exiting: %s" error
    printfn ""


let puzzleSortId (p: Puzzle) = sprintf "%4i-%2i" p.Year p.Day

let buildAutofacContainer () =
  let builder = new ContainerBuilder()

  let alreadyLoaded = 
    AppDomain.CurrentDomain.GetAssemblies()
    |> Seq.map(fun a -> a.FullName)
    |> Set.ofSeq
  let isAocRuntimeAssembly (a: Assembly) =
    a.FullName.StartsWith("Kodfodrasz.AoC")
    && not (alreadyLoaded |> Set.contains a.FullName)
    && not (a.FullName.Contains("Test", StringComparison.InvariantCultureIgnoreCase))

  let assemblies = 
    Directory.GetFiles(AppDomain.CurrentDomain.BaseDirectory, "*.dll")
    |> Seq.choose( fun p -> 
        try
          let a = Assembly.LoadFrom(p)
          Option.ofObj a
        with _ -> None)
    |> Seq.filter isAocRuntimeAssembly
    |> Seq.distinct
    |> Seq.toArray

  builder
    .RegisterAssemblyTypes(assemblies)
    .AssignableTo<ISolver>()
    .Where(fun t -> t |> AdventOfCode.hasIgnoreSolverAttribute |> not)
    .Named<ISolver>(fun solverType -> $"{Conventions.solverYear (solverType)}-{Conventions.solverDay (solverType)}")
    .AsImplementedInterfaces()
    .AsSelf()
  |> ignore

  builder.Build()

let enumerateSolvers (container: IContainer): ISolver seq =
  container.Resolve<System.Collections.Generic.IEnumerable<ISolver>>()
  |> Seq.sortBy (Puzzle.ofSolver >> puzzleSortId)

[<EntryPoint>]
let main argv =
  try
    // Set a global default Regex timeout
    AppDomain.CurrentDomain.SetData("REGEX_DEFAULT_MATCH_TIMEOUT", TimeSpan.FromSeconds(10))

    PrintBanner AocYear

    use container = buildAutofacContainer ()

    let allSolvers =
      enumerateSolvers container
      |> Seq.sortBy (Puzzle.ofSolver >> puzzleSortId)
      |> Seq.toList

    let run options input =
      let solversSeq =
        input
        |> Seq.map UserMenu.parseCommand
        |> Seq.takeWhile (fun c -> c <> UserMenu.Command.Exit)
        |> Seq.map (UserMenu.getSolvers allSolvers)

      for s in solversSeq do
        match s with
        | None -> UserMenu.invalidInput ()
        | Some solvers -> UserMenu.Solve options solvers

    let input =
      match argv with
      | [||] ->
          seq {
            while true do
              UserMenu.ListSolvers allSolvers
              yield UserMenu.userPrompt ()
          }
      | args -> args |> Seq.ofArray

    let optionsMaybe = AdventOfCode.Options.fromEnvVars ()

    match optionsMaybe with
    | Error e -> UserMenu.GoodbyeError e
    | Ok options ->
        run options input
        UserMenu.Goodbye()

    0
  with ex ->
    eprintfn "Unhandled exception occured [%s]: %s" (ex.GetType().FullName) ex.Message
    eprintfn ""
    eprintfn ""
    eprintfn "%A" ex
    1
