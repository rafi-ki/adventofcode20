module CommonTypes

type DailyPuzzle = {
    Day: int
    Lines: string []
}

type SolvePuzzle = DailyPuzzle -> string
