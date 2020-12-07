module Program

open System.IO
open CommonTypes
open adventofcode20.Resolver

let solveFor day : SolvePuzzle option =
    match day with
    | 1 -> Some DayOne.solve
    | 2 -> Some DayTwo.solve
    | 3 -> Some DayThree.solve
    | 4 -> Some DayFour.solve
    | 5 -> Some DayFive.solve
    | 6 -> Some DaySix.solve
    | 7 -> Some DaySeven.solve
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

let readDayPart (args: string []) =
    match args.Length with
    | 1 -> (args.[0] |> int, 1)
    | 2 -> (args.[0] |> int, args.[1] |> int)
    | _ -> (1, 1)


[<EntryPoint>]
let main argv =
    let (day, part) = readDayPart argv
    printfn "-- DAY %i | PART %i --" day part
    match inputFor day |> readLines with
    | Ok lines -> solvePuzzle { Day = day; Part = part; Lines = lines }
    | Error txt -> sprintf "ERROR: %s" txt
    |> printfn "%s"
    0
