module adventofcode20.Resolver

module DayOne =
    open CommonTypes
    let solve : SolvePuzzle = fun (puzzle) ->
          {
            Puzzle = puzzle
            Solution = sprintf "There are %i lines" puzzle.Lines.Length
        }
