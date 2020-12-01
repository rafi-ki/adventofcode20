module adventofcode20.Resolver

module DayOne =
    open CommonTypes

    let solve1 lines =
        let mutable answer = 0;
        for line in lines do
            let matchingLine = lines |> Array.except [|line|] |> Seq.tryFind (fun x -> x + line = 2020)
            if matchingLine.IsSome then answer <- matchingLine.Value * line
        answer |> string

    let solve2 lines =
        let mutable answer = 0;
        for line in lines do
            for second in lines |> Array.except [|line|] do
                let matchingLine = lines |> Array.except [|line;second|] |> Seq.tryFind (fun x -> x + line + second = 2020)
                if matchingLine.IsSome then answer <- matchingLine.Value * line * second
        answer |> string

    let solve puzzle =
        let lines = puzzle.Lines |> Array.map int
        if puzzle.Part = 1
        then solve1 lines
        else solve2 lines
