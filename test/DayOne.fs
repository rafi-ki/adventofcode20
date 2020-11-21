module Tests

open System
open Xunit
open CommonTypes
open adventofcode20
open Resolver


module DayOne =
    [<Fact>]
    let ``solve empty returns 0``() =
        let puzzle = {
            Day = 1
            Part = 1
            Lines = Array.empty
        }
        let result = DayOne.solve puzzle
        Assert.Equal("0", result)

    [<Fact>]
    let ``solve 1969 returns 654``() =
        let puzzle = {
            Day = 1
            Part = 1
            Lines = [|"1969"|]
        }
        let result = DayOne.solve puzzle
        Assert.Equal("654", result)

    [<Fact>]
    let ``solve 100756 returns 33583``() =
        let puzzle = {
            Day = 1
            Part = 1
            Lines = [|"100756"|]
        }
        let result = DayOne.solve puzzle
        Assert.Equal("33583", result)
