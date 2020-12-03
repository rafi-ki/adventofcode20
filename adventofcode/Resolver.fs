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

module DayThree =
    open CommonTypes

    type Coords = { X: int; Y: int }

    type Collector = {
        Field: string[]
        Coords: Coords
        Count: int
    }

    let private move x y collector =
        let coords = collector.Coords
        let movedCoords = { coords with X = coords.X + x; Y = coords.Y + y }
        { collector with Coords = movedCoords }

    let private sitsOnTree collector =
        let coords = collector.Coords
        if collector.Field.Length <= coords.Y
        then false
        else
            let line = collector.Field.[coords.Y]
            line.[coords.X % line.Length] = '#'

    let private collect collector =
        let cnt = if sitsOnTree collector then 1 else 0
        { collector with Count = collector.Count + cnt }

    let private solve1 (lines: string[])  =
        let collector = {
            Field = lines
            Coords = { X = 0; Y = 0 }
            Count = 0
        }
        let work = collect >> move 3 1
        let result = { 1..lines.Length }
                        |> Seq.fold (fun next _ -> work next) collector
        result.Count |> string

    let private solve2move x y (lines: string[]) =
        let collector = {
            Field = lines
            Coords = { X = 0; Y = 0 }
            Count = 0
        }
        let work = collect >> move x y
        let result = { 1..lines.Length }
                        |> Seq.fold (fun next _ -> work next) collector
        result.Count |> int64

    let private solve2 (lines: string[]) =
        let c1 = solve2move 1 1 lines
        let c2 = solve2move 3 1 lines
        let c3 = solve2move 5 1 lines
        let c4 = solve2move 7 1 lines
        let c5 = solve2move 1 2 lines
        c1 * c2 * c3 * c4 * c5 |> string

    let solve puzzle =
        let solve = if puzzle.Part = 1 then solve1 else solve2
        solve puzzle.Lines
