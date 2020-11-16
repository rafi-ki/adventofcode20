module CommonTypes

type DailyPuzzle = {
    Day: int
    Lines: string []
}

type SolvedPuzzle = {
    Puzzle: DailyPuzzle
    Solution: string
}

type SolvePuzzle = DailyPuzzle -> SolvedPuzzle
