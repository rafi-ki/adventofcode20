module Tests

open Xunit
open CommonTypes
open adventofcode20
open Resolver


module DayOne =

    [<Fact>]
    let ``solve example part one``() =
        let puzzle = {
            Day = 1
            Part = 1
            Lines = [|"1721";"979";"366";"299";"675";"1456"|]
        }
        let result = DayOne.solve puzzle
        Assert.Equal("514579", result)

    [<Fact>]
    let ``solve example part two``() =
        let puzzle = {
            Day = 1
            Part = 2
            Lines = [|"1721";"979";"366";"299";"675";"1456"|]
        }
        let result = DayOne.solve puzzle
        Assert.Equal("241861950", result)
