module adventofcode20.Tests

open Xunit
open Xunit.Abstractions
open CommonTypes
open adventofcode20.Resolver

type Tests(output:ITestOutputHelper) =

    [<Fact>]
    let solve_day_1() =
        let puzzle = {
            Day = 1
            Part = 1
            Lines = Array.empty
        }
        let result = DayOne.solve puzzle
        Assert.Equal("0", result)

//    [<Fact>]
//    let minus_3_from_5_should_be_2() =
//        let result = Calculator.subtract 5 3
//        write result
//        Assert.Equal(2, result)
//
//    [<Fact>]
//    let multiply_3_and_5_should_be_15() =
//        let result = Calculator.multiply 3 5
//        write result
//        Assert.Equal(15, result)
//
//    [<Fact>]
//    let divide_8_by_2_should_be_4() =
//        let result = Calculator.divide 8 2
//        write result
//        Assert.Equal((decimal)4, result)
