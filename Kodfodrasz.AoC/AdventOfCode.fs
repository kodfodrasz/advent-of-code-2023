[<AutoOpen>]
module Kodfodrasz.AoC.AdventOfCode

open System
open System.IO
open System.Net.Http

type Options =
  { DataDirectory: string
    SessionCookie: string }
  // fsharplint:disable-next-line MemberNames
  static member fromEnvVars() =
    // fsharplint:disable-next-line NonPublicValuesNames
    let AOC_DATA_DIR =
      Environment.GetEnvironmentVariable "AOC_DATA_DIR"
      |> Option.ofObj
      |> Result.ofOption "Environment variable AOC_DATA_DIR is not set!"

    // fsharplint:disable-next-line NonPublicValuesNames
    let AOC_SESSION_COOKIE =
      Environment.GetEnvironmentVariable "AOC_SESSION_COOKIE"
      |> Option.ofObj
      |> Result.ofOption "Environment variable AOC_SESSION_COOKIE is not set!"

    (AOC_DATA_DIR, AOC_SESSION_COOKIE)
    ||> Result.map2
          (fun dir cookie ->
            { DataDirectory = dir
              SessionCookie = cookie })

type Puzzle =
  { Name: string
    Year: int
    Day: int }
  // fsharplint:disable-next-line MemberNames
  static member ofSolver(solver: ISolver) =
    { Name = solver.Name
      Year = solver.Year
      Day = solver.Day }

and ISolver =
  interface
    abstract Year: int
    abstract Day: int
    abstract Name: string
    abstract Solve: Input -> Result<Solution, string> seq
  end

and Solution =
  { Year: int
    Day: int
    Part: int
    Value: string
    CalculationTime: TimeSpan }

and Input =
  { Data: string }
  // fsharplint:disable-next-line MemberNames
  static member getInput (options: Options) (puzzle: Puzzle) =
    try
      try
        Directory.CreateDirectory options.DataDirectory
        |> ignore
      with
      | _ -> failwith $"Could not create puzzle input cache directory. Check the directory {options.DataDirectory}. You can change its value by settings AOC_DATA_DIR environment variable."

      let inputPath =
        Path.Join(options.DataDirectory, $"input-{puzzle.Year}-{puzzle.Day}.txt")

      let input =
        if File.Exists inputPath then
          File.ReadAllText inputPath
        else
          use client = new HttpClient()
          client.DefaultRequestHeaders.Add("Cookie", $"session={options.SessionCookie}")

          let url =
            $"https://adventofcode.com/{puzzle.Year}/day/{puzzle.Day}/input"

          let response =
            try
              client.GetStringAsync(url)
              |> Async.AwaitTask
              |> Async.RunSynchronously
            with
            | _ -> failwith $"Could not download puzzle input. Check the session cookie in AOC_SESSION_COOKIE environment variable."

          File.WriteAllText(inputPath, response)

          response

      Ok { Data = input }
    with ex -> Error $"Unexpected error [{ex.GetType().FullName}]: {ex.Message}"

[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Module, AllowMultiple = false)>]
type IgnoreSolverAttribute(note: string) =
   inherit System.Attribute()
   member val Note: string = note with get, set


let rec hasIgnoreSolverAttribute (t: Type) =
  let attributes = t.GetCustomAttributes(typeof<IgnoreSolverAttribute>, true)
  if attributes.Length > 0 then true
  else
      match t.DeclaringType with
      | null -> false
      | dt -> hasIgnoreSolverAttribute dt

module Conventions =
  let private isSolver (solverType: Type) =
    let interfaceType = typeof<ISolver>
    interfaceType.IsAssignableFrom(solverType)

  let private isNotSolver = isSolver >> not

  let private tryGetInteger regex str =
    let m =
      System.Text.RegularExpressions.Regex.Match(str, regex)

    if m.Success then
      let candidate = m.Groups.[1].Value

      match (Int32.TryParse candidate) with
      | true, year -> Some year
      | false, _ -> None
    else
      None

  let solverYear (solverType: Type) =
    if solverType |> isNotSolver then
      Error $"Not solver type: {solverType.FullName} is not a subclass if ISolver interface"
    else
      solverType.FullName.Split('.')
      |> Seq.choose (tryGetInteger @"Year(\d{4})")
      |> Seq.tryHead
      |> function
      | None -> Error $"Could not determine year of Solver for type {solverType.FullName}"
      | Some year -> Ok year

  let solverDay (solverType: Type) =
    if solverType |> isNotSolver then
      Error $"Not solver type: {solverType.FullName} is not a subclass if ISolver interface"
    else
      solverType.FullName.Split('.')
      |> Seq.choose (tryGetInteger @"Day(\d+)")
      |> Seq.tryHead
      |> function
      | None -> Error $"Could not determine day of Solver for type {solverType.FullName}"
      | Some year -> Ok year

[<AbstractClass>]
type SolverBase(name) as this =
  do if (String.IsNullOrWhiteSpace name) then failwith "No solver name was provided!"

  let year =
    this.GetType()
    |> Conventions.solverYear
    |> Result.get

  let day =
    this.GetType()
    |> Conventions.solverDay
    |> Result.get

  member this.DoSolve (parser: string -> Result<'I, string>) (calculators: ('I -> Result<_, string>) list) input =
    seq {
      let execute part calculate num =
        let sw = System.Diagnostics.Stopwatch.StartNew()

        calculate num
        |> Result.map
             (fun value ->
               { Year = this.Year
                 Day = this.Day
                 Part = part
                 CalculationTime = sw.Elapsed
                 Value = string value })

      let parsedInputMaybe =
        try
          input.Data |> parser
        with ex ->
          Error
          <| sprintf "Input parsing error [%s]:  %s" (ex.GetType().FullName) ex.Message

      match parsedInputMaybe with
      | Error e -> yield Error e
      | Ok numbers ->
          for (i, calc) in (List.indexed calculators) do
            let part = i + 1

            let answer =
              try
                numbers |> execute part calc
              with ex ->
                Error
                <| sprintf "Calculation error [%s]:  %s" (ex.GetType().FullName) ex.Message

            yield answer
    }

  member this.Year = year
  member this.Day = day
  member this.Name = name
  abstract Solve: Input -> Result<Solution, string> seq

  interface ISolver with
    member this.Year = this.Year
    member this.Day = this.Day
    member this.Name = this.Name
    member this.Solve input = this.Solve input

let note msg = 
  if msg |> String.isNullOrWhiteSpace then Result.map (sprintf "%A")
  else Result.map (fun i -> sprintf "%A (%s)" i msg)
