module CommonTypes

type DailyPuzzle = {
    Day: int
    Part: int
    Lines: string []
}

type SolvePuzzle = DailyPuzzle -> string

let (./.) x y = (x |> double) / (y |> double)
