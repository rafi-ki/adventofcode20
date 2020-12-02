module adventofcode20.Resolver

module DayOne =
    open CommonTypes

    let solve1 lines =
        let mutable answer = 0;
        for line in lines do
            let matchingLine = lines |> Array.except [|line|] |> Seq.tryFind (fun x -> x + line = 2020)
            matchingLine |> Option.map (fun x -> answer <- x * line) |> ignore
        answer |> string

    let solve2 lines =
        let mutable answer = 0;
        for first in lines do
            for second in lines |> Array.except [|first|] do
                let matchingLine = lines |> Array.except [|first;second|] |> Seq.tryFind (fun x -> x + first + second = 2020)
                matchingLine |> Option.map (fun x -> answer <- x * first * second) |> ignore
        answer |> string

    let solve puzzle =
        let solve = if puzzle.Part = 1 then solve1 else solve2
        puzzle.Lines |> Array.map int |> solve

module DayTwo =
    open CommonTypes

    type PwdPolicy = {
        First: int
        Second: int
        Letter: char
        Password: string
    }

    let private parseLine (line: string) =
        let split = line.Split " "
        let range = split.[0].Split "-"
        {
            First = int range.[0]
            Second = int range.[1]
            Letter = split.[1].Substring(0, 1) |> char
            Password = split.[2]
        }

    let private isValid (line: PwdPolicy) =
        let countAppearance x = Seq.filter ((=) x) >> Seq.length
        let count = countAppearance line.Letter line.Password
        count >= line.First && count <= line.Second

    let private isValid2 (line: PwdPolicy) =
        let matchesFirst = line.Password.[line.First - 1] = line.Letter
        let matchesSecond = line.Password.[line.Second - 1] = line.Letter
        (matchesFirst && not matchesSecond) || (matchesSecond && not matchesFirst)

    let private solve1 policies =
        policies |> Array.filter isValid |> Array.length

    let private solve2 policies =
        policies |> Array.filter isValid2 |> Array.length

    let solve puzzle =
        let solve = if puzzle.Part = 1 then solve1 else solve2
        let policies = puzzle.Lines |> Array.map parseLine
        solve policies |> string
