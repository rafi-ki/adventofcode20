module Program

open System.IO
open CommonTypes
open adventofcode20.Resolver

let solveFor day : SolvePuzzle option =
    match day with
    | 1 -> Some DayOne.solve
    | _ -> None

let solvePuzzle (puzzle: DailyPuzzle) =
    match solveFor puzzle.Day with
    | Some solve -> solve puzzle
    | None ->  "no solve function defined"

let inputFor day = sprintf "input/day%i.txt" day

let readLines (file: string) =
    match File.Exists file with
    | true -> Ok (File.ReadLines file |> Seq.toArray)
    | false -> Error (sprintf "Input file %s does not exist" file)

[<EntryPoint>]
let main argv =
    match argv.Length with
        0 -> printfn "Please provide a day"
        | _ ->
            let day = argv.[0] |> int
            printfn "-- DAY %i --" day
            match inputFor day |> readLines with
            | Ok lines -> solvePuzzle { Day = day; Lines = lines }
            | Error txt -> sprintf "ERROR: %s" txt
            |> printfn "%s"
    0
