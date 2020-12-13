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
        | Nop _ -> { field with Index = field.Index + 1 }
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
        let slice = values.[i-preamble..i-1]
        let mutable result = false;
        for v in slice do
            let slicedOut = slice |> Array.except [|v|] |> Array.tryFind (fun item -> item + v = x)
            slicedOut |> Option.map (fun _ -> result <- true) |> ignore
        result

    let solve1 (values: int64[]) =
        let last = values
                    |> Array.skip preamble
                    |> Array.mapi (fun i x -> (i+preamble, x))
                    |> Seq.takeWhile (fun x -> addsUp (fst x) (snd x) values)
                    |> Seq.last
        let next = values.[(fst last)+1]
        next |> string

    let sumWhile predicate (values: (int * int64)[]) =
        let mutable result = 0L
        let mutable i = 0
        let mutable originalIndex = 0
        while predicate result do
            let (index, value) = values.[i]
            result <- result + value
            originalIndex <- index
            i <- i + 1
        (result, i-1, originalIndex)

    let findContiguous (solution: int64) (values: int64[]) =
        let mutable result = [||]
        let mutable visited = [||]
        let indexedValues = values |> Array.mapi (fun i x -> (i, x))
        for v in indexedValues do
            let exceptValues = indexedValues |> Array.except visited
            let (sum, index, originalIndex) = exceptValues |> sumWhile (fun x -> x < solution)
            if sum = solution && index > 1 then
                result <- values.[originalIndex - index..originalIndex]
            visited <- visited |> Array.append [|v|]
        result

    let solve2 (values: int64[]) =
        let solution1 = solve1 values |> int64
        let contiguous = findContiguous solution1 values
        let min = contiguous |> Seq.min
        let max = contiguous |> Seq.max
        min + max |> string

    let solve puzzle =
        let values = puzzle.Lines |> Array.map int64
        values |> if puzzle.Part = 1 then solve1 else solve2

module DayTen =
    open CommonTypes

    let sandwich values = Array.concat [| [|0|]; values; [|(Array.last values)+3|] |]

    let solve1 values =
        let differences =
            Array.sort values |> sandwich
            |> Array.pairwise
            |> Array.map (fun (x, y) -> y-x)
        let difference1 = differences |> Array.filter ((=) 1) |> Array.length
        let difference3 = differences |> Array.filter ((=) 3) |> Array.length
        difference1 * difference3 |> string

    let cache = Dictionary<int, int64>()

    let rec move index (values: int[]) =
        match cache.TryGetValue index with
        | true, v -> v
        | false, _ ->
            if index = values.Length-1 then
                1L
            else
                let valueAt = values.[index]
                let result =
                    values.[index+1..index+3]
                    |> Array.mapi (fun i x -> (index+i+1, x))
                    |> Array.filter (fun (_, x) -> x - valueAt <= 3)
                    |> Array.sumBy (fun (i, _) -> move i values)
                cache.Add(index, result)
                result

    let solve2 values =
        let complete = Array.sort values |> sandwich
        move 0 complete |> string

    let solve puzzle =
        let values = puzzle.Lines |> Array.map int
        if puzzle.Part = 1 then solve1 values else solve2 values

module DayEleven =
    open CommonTypes

    type Usage = Floor | Empty | Occupied

//    let areaDimension = (97, 89)
    let areaDimension = (9, 9)

    type Direction = Up | Down | Left | Right | UpRight | UpLeft | DownRight | DownLeft

    type Seat = {
        Col: int
        Row: int
        Usage: Usage
    }

    type WaitingArea = Seat[]

    let private parseSeat col row c =
        let usage = match c with
                        | '.' -> Floor
                        | 'L' -> Empty
                        | '#' -> Occupied
                        | _ -> failwith "Unknown char in waiting area"
        { Col = col; Row = row; Usage = usage }

    let parseWaitingArea lines =
        lines |> Array.mapi
                     (fun row line -> line |> Array.mapi
                                                (fun col c -> parseSeat col row c))
                     |> Array.collect id


    let jointCache = Dictionary<(int*int), seq<int*int>>()

    let crossJoint row col =
        match jointCache.TryGetValue ((row, col)) with
        | true, v -> v
        | false, _ ->
            let (maxRows, maxCols) = areaDimension
            let rows = [Math.Max(row - 1, 0) .. Math.Min(row + 1, maxRows)]
            let cols = [Math.Max(col - 1, 0) .. Math.Min(col + 1, maxCols)]
            rows
            |> Seq.collect (fun r -> cols |> Seq.map (fun c -> r, c))
            |> Seq.filter (fun (r, c) -> r <> row || c <> col) // filter own seat

    let adjacentSeats (waitingArea: WaitingArea) (seat: Seat) =
        crossJoint seat.Row seat.Col
        |> Seq.map (fun (row, col) -> waitingArea |> Array.find (fun x -> x.Row = row && x.Col = col))
        |> Array.ofSeq

    let nextNoFloorSeat (waitingArea: WaitingArea) (seat: Seat) direction =
        let (maxRow, maxCol) = areaDimension
        match direction with
        | Up ->
            if seat.Row = 0 then
                None
            else
                let upwardsRows = seq [seat.Row-1..0]
                upwardsRows
                |> Seq.map (fun x -> waitingArea |> Array.find (fun y -> y.Row = x && y.Col = seat.Col))
                |> Seq.takeWhile (fun x -> x.Usage = Floor)
                |> Seq.last
                |> Some
        | Down ->
            if seat.Row = maxRow then
                None
            else
                let downwardsRow = seq [seat.Row+1..maxRow]
                downwardsRow
                |> Seq.map (fun x -> waitingArea |> Array.find (fun y -> y.Row = x && y.Col = seat.Col))
                |> Seq.takeWhile (fun x -> x.Usage = Floor)
                |> Seq.last
                |> Some
        | Right ->
            if seat.Col = maxCol then
                None
            else
                let rightCols = seq [seat.Col+1..maxCol]
                rightCols
                |> Seq.map (fun x -> waitingArea |> Array.find (fun y -> y.Row = seat.Row && y.Col = x))
                |> Seq.takeWhile (fun x -> x.Usage = Floor)
                |> Seq.last
                |> Some
        | Left ->
            if seat.Col = 0 then
                None
            else
                let rightCols = seq [seat.Col-1..0]
                rightCols
                |> Seq.map (fun x -> waitingArea |> Array.find (fun y -> y.Row = seat.Row && y.Col = x))
                |> Seq.takeWhile (fun x -> x.Usage = Floor)
                |> Seq.last
                |> Some
        | UpLeft ->
            if seat.Row = 0 || seat.Col = 0 then
                None
            else
                let upwardsRows = seq [seat.Row-1..0]
                upwardsRows
                |> Seq.map (fun x -> waitingArea |> Array.find (fun y -> y.Row = x && y.Col = seat.Col))
                |> Seq.takeWhile (fun x -> x.Usage = Floor)
                |> Seq.last
                |> Some

    let lookaroundSeats (waitingArea: WaitingArea) (seat: Seat) =
        ""

    let applyRule seat (adjacentSeats: Seat[]) =
        let newUsage =
            match seat.Usage with
            | Floor -> Floor
            | Empty ->
                if (adjacentSeats |> Seq.filter (fun x -> x.Usage = Occupied) |> Seq.length) = 0 then
                    Occupied
                else
                    Empty
            | Occupied ->
                if (adjacentSeats |> Seq.filter (fun x -> x.Usage = Occupied) |> Seq.length) > 3 then
                    Empty
                else
                    Occupied
        { seat with Usage = newUsage }

    let runRound (waitingArea: WaitingArea) =
        waitingArea
        |> Array.map (fun x -> applyRule x (adjacentSeats waitingArea x))

    let solve1 waitingArea =
        let mutable mutableArea = runRound waitingArea
        let mutable prev = waitingArea
        while prev <> mutableArea do
            prev <- mutableArea
            mutableArea <- runRound prev
        mutableArea
        |> Array.filter (fun x -> x.Usage = Occupied)
        |> Array.length
        |> string

    let solve puzzle =
        let waitingArea = parseWaitingArea (puzzle.Lines |> Array.map (fun x -> Seq.toArray x))
        if puzzle.Part = 1 then solve1 waitingArea else "2"

module DayTwelfth =
    open CommonTypes

    type Position = {
        X: int
        Y: int
    }

    type Direction = North | South | East | West

    type Ship = {
        FacingDirection: Direction
        Position: Position
        Waypoint: Position
    }

    let parseInstruction (line: string) =
        let c = line.Substring(0, 1) |> char
        let v = line.Substring(1) |> int
        (c, v)

    let parseInstructions lines = lines |> Seq.map parseInstruction

    let moveNorth v ship=
        let waypoint = { ship.Waypoint with Y = ship.Waypoint.Y - v }
        { ship with Waypoint = waypoint }

    let moveSouth v ship =
        let waypoint = { ship.Waypoint with Y = ship.Waypoint.Y + v }
        { ship with Waypoint = waypoint }

    let moveEast v ship =
        let waypoint = { ship.Waypoint with X = ship.Waypoint.X + v }
        { ship with Waypoint = waypoint }

    let moveWest v ship =
        let waypoint = { ship.Waypoint with X = ship.Waypoint.X - v }
        { ship with Waypoint = waypoint }

    let moveForward v ship =
        let xMovement = ship.Waypoint.X * v
        let yMovement = ship.Waypoint.Y * v
        let position = { ship.Position with X = ship.Position.X + xMovement; Y = ship.Position.Y + yMovement }
        { ship with Position = position }

    let turnAround waypoint = { X = -1 * waypoint.X; Y = -1 * waypoint.Y }

    let turnLeft v ship =
        let waypoint =
            match v with
            | 90 -> { X = ship.Waypoint.Y; Y = -1 * ship.Waypoint.X }
            | 180 -> turnAround ship.Waypoint
            | 270 -> { X = -1 * ship.Waypoint.Y; Y = ship.Waypoint.X }
            | _ -> failwith "unknown degrees"
        { ship with Waypoint = waypoint }

    let turnRight v ship =
        let waypoint =
            match v with
            | 90 -> { X = -1 * ship.Waypoint.Y; Y = ship.Waypoint.X }
            | 180 -> turnAround ship.Waypoint
            | 270 -> { X = ship.Waypoint.Y; Y = -1 * ship.Waypoint.X }
            | _ -> failwith "unknown degrees"
        { ship with Waypoint = waypoint }

    let move instruction =
        match instruction with
        | ('N', v) -> moveNorth v
        | ('S', v) -> moveSouth v
        | ('E', v) -> moveEast v
        | ('W', v) -> moveWest v
        | ('F', v) -> moveForward v
        | ('L', v) -> turnLeft v
        | ('R', v) -> turnRight v
        | _ -> failwith "unknown instruction"

    let solve1 instructions =
        let ship = {
            FacingDirection = East
            Position = { X = 0; Y = 0; }
            Waypoint = { X = 0; Y = 0; }
        }
        let move =
            instructions
            |> Seq.map (fun x -> move x)
            |> Seq.reduce (>>)
        let finalShip = move ship
        Math.Abs finalShip.Position.X + Math.Abs finalShip.Position.Y |> string

    let solve2 instructions =
        let ship = {
            FacingDirection = East
            Position = { X = 0; Y = 0; }
            Waypoint = { X = 10; Y = -1; }
        }
        let move =
            instructions
            |> Seq.map (fun x -> move x)
            |> Seq.reduce (>>)
        let finalShip = move ship
        Math.Abs finalShip.Position.X + Math.Abs finalShip.Position.Y |> string

    let solve puzzle =
        let instructions = parseInstructions puzzle.Lines
        if puzzle.Part = 1 then solve1 instructions else solve2 instructions

module DayThirteen =
    open CommonTypes

    let busArrivalsTillLeaving (arrival: int) original x =
        if arrival <= x - original then
            None
        else
            Some(x, x + original)

    let unfoldBus arrival (busId: int) =
        let busArrivals = busArrivalsTillLeaving arrival busId
        Seq.unfold (fun x -> busArrivals x) busId |> Array.ofSeq

    let solve1 (arrival: int) (buses: int[]) =
        let unfoldBusForArrival = unfoldBus arrival
        let allBusArrivals =
            buses |> Array.map unfoldBusForArrival
        let (busId, firstBusLeavingTime) =
            allBusArrivals
            |> Array.map (fun x -> (x.[0], Array.last x))
            |> Array.minBy (fun (_, last) -> last)
        let waitingTime = firstBusLeavingTime - arrival
        waitingTime * busId |> string

    let noX x = x <> "x"

    let solve puzzle =
        let arrival = puzzle.Lines.[0] |> int
        let buses = puzzle.Lines.[1].Split "," |> Array.filter noX
        if puzzle.Part = 1 then
            solve1 arrival (buses |> Array.map int)
        else
            "2"
