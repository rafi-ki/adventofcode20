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

    let colRange = { Low = 0; High = 7 }
    let rowRange = { Low = 0; High = 127 }

    let takeUpper range =
        let dif = ((range.High - range.Low) / 2) + 1
        { Low = range.Low + dif; High = range.High }

    let takeLower range =
        let dif = ((range.High - range.Low) / 2) + 1
        { Low = range.Low; High = range.High - dif }

    let letterToHalfDown x =
        match x with
        | 'F' | 'L' -> takeLower
        | 'B' | 'R' -> takeUpper
        | _ -> failwith "unknown"

    let breakDown (letters: seq<char>) (range: Range) =
        let fold =
            letters
            |> Seq.map letterToHalfDown
            |> Seq.reduce (>>)
        (fold range).Low

    let col (line: string) = colRange |> breakDown (line |> Seq.skip 7)

    let row (line: string) = rowRange |> breakDown (line |> Seq.take 7)

    let seat (line: string) = { Row = row line; Col = col line }

    let seatId (seat: Seat) = seat.Row * 8 + seat.Col

    let private hasEmptyPrevAndNext id (idsOnPlane: int[]) =
        let set = idsOnPlane |> Set.ofArray
        set.Contains (id-1) && set.Contains (id+1)

    let solve puzzle =
        let seatIdsOnPlane = puzzle.Lines |> Array.map (seat >> seatId)
        if puzzle.Part = 1 then
            seatIdsOnPlane
            |> Array.max
            |> string
        else
            let emptySeatIds = [|8 .. (127 * 8 - 1)|] |> Array.except seatIdsOnPlane
            emptySeatIds
            |> Array.find (fun x -> hasEmptyPrevAndNext x seatIdsOnPlane)
            |> string

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
        |> Array.map (intersectionFor >> Set.count)
        |> Array.sum
        |> string

    let solve puzzle =
        let solve = if puzzle.Part = 1 then solve1 else solve2
        solve puzzle.Lines

module DaySeven =
    open CommonTypes

    type Bag = {
        Color: string
        Content: Bag[]
        Count: int
    }

    let mutable topLevelBags = Array.empty

    let parseSingleContent (content: string) =
        let noBags = content.Replace(" bags", "").Replace(" bag", "").Replace(".", "")
        let split = noBags.Split " "
        let color = split |> Array.skip 1 |> String.concat " "
        { Color = color; Content = Array.empty; Count = split.[0] |> int }

    let parseContent (content: string) =
        if content = "no other bags." then
            Array.empty
        else
            content.Split ", " |> Array.map parseSingleContent

    let parseBag (line: string) =
        let split = line.Split " contain "
        let color = split.[0].Replace("bags", "").Trim()
        let content = parseContent split.[1]
        { Color = color; Content = content; Count = 1 }

    let isShinyGold bag = bag.Color = "shiny gold"

    let rec containsShinyGold bag =
        let contains = bag.Content |> Array.filter isShinyGold |> Array.length > 0
        if bag.Content |> Array.isEmpty then
            false
        else
            let childContains =
                bag.Content
                |> Array.map (fun x -> topLevelBags |> Array.tryFind (fun y -> y.Color = x.Color))
                |> Array.choose id
                |> Array.filter containsShinyGold
                |> Array.length
            contains || childContains > 0

    let solve1 puzzle =
        topLevelBags <- puzzle.Lines |> Array.map parseBag
        topLevelBags
        |> Array.filter containsShinyGold
        |> Array.length
        |> string

    let rec countContent layer bag =
        if Array.isEmpty bag.Content then
            bag.Count
        else
            let contentCount =
                bag.Content
                |> Array.map (fun x -> x.Count)
                |> Array.sum
            let recCount =
                bag.Content
                |> Array.map (fun x -> topLevelBags |> Array.find (fun y -> y.Color = x.Color))
                |> Array.map (fun x -> countContent (layer+1) x)
                |> Array.sum
            (bag.Count  + contentCount + (recCount * layer))

    let solve2 puzzle =
        topLevelBags <- puzzle.Lines |> Array.map parseBag
        let shinyGold = topLevelBags |> Array.find isShinyGold
        countContent 1 shinyGold |> string

    let solve puzzle =
        if puzzle.Part = 1 then solve1 puzzle else solve2 puzzle

module DayEight =
    open CommonTypes

    type Instruction =
        | Acc of int
        | Jmp of int
        | Nop of int

    type BootLoader = {
        Accumulator: int
        Index: int
        Visited: int[]
        Infinity: bool
    }

    let initState = { Accumulator = 0
                      Index = 0
                      Visited = [||]
                      Infinity = false }

    let parseInstruction (line: string) =
        let split = line.Split " "
        let value = int split.[1]
        match split.[0] with
        | "nop" -> Nop value
        | "acc" -> Acc value
        | "jmp" -> Jmp value
        | _ -> failwith "unrecognized instruction"

    let private executeInstruction instruction field =
        match instruction with
        | Nop value -> { field with Index = field.Index + 1 }
        | Acc value -> { field with Accumulator = field.Accumulator + value; Index = field.Index + 1 }
        | Jmp value -> { field with Index = field.Index + value }

    let rec move instruction bootLoader (instructions: Instruction[]) =
        if bootLoader.Visited |> Array.contains bootLoader.Index then
            { bootLoader with Infinity = true }
        else
            let newBootloader = executeInstruction instruction bootLoader
            if newBootloader.Index = instructions.Length then
                newBootloader
            else
                move instructions.[newBootloader.Index]
                    { newBootloader with Visited = Array.append bootLoader.Visited [|bootLoader.Index|] } instructions

    let solve1 lines =
        let instructions = lines |> Array.map parseInstruction
        let result = move instructions.[0] initState instructions
        result.Accumulator |> string

    let flip instruction =
        match instruction with
        | Jmp x -> Nop x
        | Nop x -> Jmp x
        | Acc x -> Acc x

    let solve2 lines =
        let instructions = lines |> Array.map parseInstruction
        let runInstructions i toBeChanged =
            let changedInstructions = Array.copy instructions
            changedInstructions.[i] <- flip toBeChanged
            move changedInstructions.[0] initState changedInstructions
        let finiteBootloader =
                  instructions
                  |> Array.mapi runInstructions
                  |> Array.find (fun x -> not x.Infinity)
        finiteBootloader.Accumulator |> string

    let solve puzzle =
        puzzle.Lines
        |> if puzzle.Part = 1 then solve1 else solve2

module DayNine =
    open CommonTypes

    let preamble = 25

    let addsUp i x (values: int64[]) =
        let slice = values.[i-preamble ..i-1]
        let mutable result = false;
        for v in slice do
            let slicedOut = slice |> Array.except [|v|] |> Array.tryFind (fun item -> item + v = x)
            slicedOut |> Option.map (fun x -> result <- true) |> ignore
        result

    let solve1 (values: int64[]) =
        let last = values
                    |> Array.skip preamble
                    |> Array.mapi (fun i x -> (i+preamble, x))
                    |> Seq.takeWhile (fun x -> addsUp (fst x) (snd x) values)
                    |> Seq.last
        let next = values.[(fst last)+1]
        next |> string

    let solve puzzle =
        let values = puzzle.Lines |> Array.map int64
        if puzzle.Part = 1 then solve1 values else "2"
