module Program

open System.IO
open CommonTypes
open adventofcode20.Resolver

let createPuzzleResolver day : SolvePuzzle option =
    match day with
    | 1 -> Some DayOne.solve
    | _ -> None

let solvePuzzle (puzzle: DailyPuzzle) =
    let solveOption = createPuzzleResolver puzzle.Day
    match solveOption with
    | Some solve -> solve puzzle
    | None -> {
            Puzzle = puzzle
            Solution = "no solve function defined"
        }

let dailyLines (day: int) =
    let path = sprintf "input/day%i.txt" day
    match File.Exists path with
    | true -> Ok (File.ReadLines path |> Seq.toArray)
    | false -> Error (sprintf "Input file %s does not exist" path)

[<EntryPoint>]
let main argv =
    match argv.Length with
        0 -> printfn "Please provide a day"
        | _ ->
            let day = argv.[0] |> int
            printfn "-- DAY %i --" day
            let linesOption = dailyLines day
            match linesOption with
            | Ok lines ->
                let solved = solvePuzzle { Day = day; Lines = lines }
                printfn "%s" solved.Solution
            | Error txt -> printfn "ERROR: %s" txt
    0
