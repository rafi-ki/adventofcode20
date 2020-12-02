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
    }

    type Pwd = string

    type ParsedLine = PwdPolicy * Pwd

    let private parseLine (line: string): ParsedLine =
        let split = line.Split " "
        let range = split.[0].Split "-"
        let pwdPolicy = {
            First = int range.[0]
            Second = int range.[1]
            Letter = split.[1].Substring(0, 1) |> char
        }
        (pwdPolicy, split.[2])

    let private countChar c = Seq.filter ((=) c) >> Seq.length

    let private isValid (line: ParsedLine) =
        let pwdPolicy, pwd = line
        let count = countChar pwdPolicy.Letter pwd
        count >= pwdPolicy.First && count <= pwdPolicy.Second

    let private isValid2 (line: ParsedLine) =
        let pwdPolicy, pwd = line
        let matchesFirst = pwd.[pwdPolicy.First - 1] = pwdPolicy.Letter
        let matchesSecond = pwd.[pwdPolicy.Second - 1] = pwdPolicy.Letter
        (matchesFirst && not matchesSecond) || (matchesSecond && not matchesFirst)

    let solve puzzle =
        let isValid = if puzzle.Part = 1 then isValid else isValid2
        let policies = puzzle.Lines |> Array.map parseLine
        policies |> Array.filter isValid |> Array.length |> string
