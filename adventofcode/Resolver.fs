module adventofcode20.Resolver

open System
open System.Collections.Generic
open System.Text.RegularExpressions

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

module DayFour =
    open CommonTypes

    type Passport = {
        Id: string
        BirthYear: int
        IssueYear: int
        ExpirationYear: int
        Height: string
        HairColor: string
        EyeColor: string
        CountryId: string option
    }

    let private parsePassport (line: string) : Passport option =
        let toKvPair (x: string) =
            let split = x.Split ":"
            KeyValuePair(split.[0], split.[1])
        let kvPairs = line.Split " " |> Seq.map toKvPair

        let byr = kvPairs |> Seq.tryFind (fun x -> x.Key = "byr")
        let iyr = kvPairs |> Seq.tryFind (fun x -> x.Key = "iyr")
        let eyr = kvPairs |> Seq.tryFind (fun x -> x.Key = "eyr")
        let hgt = kvPairs |> Seq.tryFind (fun x -> x.Key = "hgt")
        let hcl = kvPairs |> Seq.tryFind (fun x -> x.Key = "hcl")
        let ecl = kvPairs |> Seq.tryFind (fun x -> x.Key = "ecl")
        let id = kvPairs |> Seq.tryFind (fun x -> x.Key = "pid")
        let cid = kvPairs |> Seq.tryFind (fun x -> x.Key = "cid")

        if byr.IsSome && iyr.IsSome && eyr.IsSome && hgt.IsSome && hcl.IsSome && ecl.IsSome && id.IsSome then
            {
                Id = id.Value.Value
                BirthYear = byr.Value.Value |> int
                IssueYear = iyr.Value.Value |> int
                ExpirationYear = eyr.Value.Value |> int
                Height = hgt.Value.Value
                HairColor = hcl.Value.Value
                EyeColor = ecl.Value.Value
                CountryId = cid |> Option.map (fun x -> x.Value) }
            |> Some
        else
            None

    let private validateBirthYear (pp: Passport) =
        match pp.BirthYear >= 1920 && pp.BirthYear <= 2002 with
        | true -> Ok pp
        | false -> Error "BirthYea invalid"

    let private validateIssueYear (pp: Passport) =
        match pp.IssueYear >= 2010 && pp.IssueYear <= 2020 with
        | true -> Ok pp
        | false -> Error "IssueYear invalid"

    let private validateExpirationYear (pp: Passport) =
        match pp.ExpirationYear >= 2020 && pp.ExpirationYear <= 2030 with
        | true -> Ok pp
        | false -> Error "ExpirationYear invalid"

    let private validateHeight (pp: Passport): Result<Passport, string> =
        match pp.Height.Substring(pp.Height.Length-2, 2) with
        | "cm" ->
            let value = pp.Height.Substring(0, pp.Height.Length-2) |> int
            if value >= 150 && value <= 193 then
                Ok pp
            else
                Error "Height invalid"
        | "in" ->
            let value = pp.Height.Substring(0, pp.Height.Length-2) |> int
            if value >= 59 && value <= 76 then
                Ok pp
            else
                Error "Height invalid"
        | _ -> Error "Height invalid"

    let private validateHairColor (pp: Passport) =
        let m = Regex.Match(pp.HairColor, "^#[a-f0-9]{6}")
        match m.Success with
        | true -> Ok pp
        | false -> Error "HairColor invalid"

    let private validateEyeColor (pp: Passport) =
        let m = Regex.Match(pp.EyeColor, "^(?:amb|blu|brn|gry|grn|hzl|oth)$")
        match m.Success with
        | true -> Ok pp
        | false -> Error "EyeColor invalid"

    let private validateId (pp: Passport) =
        let m = Regex.Match(pp.Id, "^[0-9]{9}$")
        match m.Success with
        | true -> Ok pp
        | false -> Error "Id invalid"

    let private validatePassport (pp: Passport): Result<Passport, string> =
        Ok pp
        |> Result.bind validateBirthYear
        |> Result.bind validateIssueYear
        |> Result.bind validateExpirationYear
        |> Result.bind validateHeight
        |> Result.bind validateHairColor
        |> Result.bind validateEyeColor
        |> Result.bind validateId

    let solve puzzle =
        let combined = puzzle.Lines |> String.concat " "
        let split = combined.Split "  "
        let passports = split |> Array.map parsePassport |> Array.choose id
        if puzzle.Part = 1 then
            string passports.Length
        else
            let mapOk x = match x with | Ok pp -> Some pp | Error _ -> None
            passports
            |> Array.map validatePassport
            |> Array.choose mapOk
            |> Array.length
            |> string

module DayFive =
    open CommonTypes

    type Range = {
        Low: int
        High: int
    }

    type Seat = {
        Row: int
        Col: int
    }

    let takeUpper range =
        let dif = ((range.High - range.Low) / 2) + 1
        { Low = range.Low + dif; High = range.High }

    let takeLower range =
        let dif = ((range.High - range.Low) / 2) + 1
        { Low = range.Low; High = range.High - dif }

    let letterToUpDown x =
        match x with
        | 'F' -> takeLower
        | 'B' -> takeUpper
        | _ -> failwith "unknown"

    let letterToRightLeft x =
         match x with
         | 'L' -> takeLower
         | 'R' -> takeUpper
         | _ -> failwith "unknown"

    let col (line: string) =
        let range = { Low = 0; High = 7 }
        let fold =
            line |> Seq.skip 7
            |> Seq.map letterToRightLeft
            |> Seq.reduce (>>)
        (fold range).Low

    let row (line: string) =
        let range = { Low = 0; High = 127 }
        let fold =
            line |> Seq.take 7
            |> Seq.map letterToUpDown
            |> Seq.reduce (>>)
        (fold range).Low

    let seat (line: string) = { Row = row line; Col = col line }

    let seatId (seat: Seat) = seat.Row * 8 + seat.Col

    let allSeats =
        let mutable result = []
        for i in 0 .. 7 do
            for j in 0 .. 127 do
                result <-  { Row = j; Col = i } :: result
        result

    let solve puzzle =
        if puzzle.Part = 1 then
            puzzle.Lines
            |> Array.map (fun x -> x |> seat |> seatId)
            |> Array.max
            |> string
        else
            let seatsOnPlane = puzzle.Lines |> Array.map seat |> List.ofArray
            let notFirstOrLastRow x = x.Row > 0 && x.Row < 127
            let remainingSeats =
                allSeats
                |> List.except seatsOnPlane
                |> List.filter notFirstOrLastRow
                |> List.map seatId
                |> List.sort
            remainingSeats.Length |> string

module DaySix =
    open CommonTypes

    let solve1 lines =
        let combined = lines |> String.concat " "
        let grouped = combined.Split "  " |> Array.map (fun x -> x.Replace(" ", ""))
        grouped
        |> Array.map (fun x -> Seq.distinct x |> Array.ofSeq |> Array.length)
        |> Array.sum
        |> string

    let intersectionFor (value: string) =
        value.Split " "
        |> Seq.map (fun x -> x.ToCharArray() |> Set.ofArray)
        |> Seq.reduce Set.intersect

    let solve2 lines =
        let combined = lines |> String.concat " "
        let grouped = combined.Split "  "
        grouped
        |> Array.map intersectionFor
        |> Array.map (fun x -> x.Count)
        |> Array.sum
        |> string

    let solve puzzle =
        let solve = if puzzle.Part = 1 then solve1 else solve2
        solve puzzle.Lines
