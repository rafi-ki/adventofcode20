module ResolverTests

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

module DayTwo =

    [<Fact>]
    let ``solve example part one``() =
        let puzzle = {
            Day = 2
            Part = 1
            Lines = [|"1-3 a: abcde";"1-3 b: cdefg";"2-9 c: ccccccccc"|]
        }
        let result = DayTwo.solve puzzle
        Assert.Equal("2", result)

    [<Fact>]
    let ``solve example part two``() =
        let puzzle = {
            Day = 2
            Part = 2
            Lines = [|"1-3 a: abcde";"1-3 b: cdefg";"2-9 c: ccccccccc"|]
        }
        let result = DayTwo.solve puzzle
        Assert.Equal("1", result)

module DayThree =

    [<Fact>]
    let ``solve example part one``() =
        let puzzle = {
            Day = 3
            Part = 1
            Lines = [|"..##.......";
                      "#...#...#..";
                      ".#....#..#.";
                      "..#.#...#.#";
                      ".#...##..#.";
                      "..#.##.....";
                      ".#.#.#....#";
                      ".#........#";
                      "#.##...#...";
                      "#...##....#";
                      ".#..#...#.#"|]
        }
        let result = DayThree.solve puzzle
        Assert.Equal("7", result)

    [<Fact>]
    let ``solve example part two``() =
        let puzzle = {
            Day = 3
            Part = 2
            Lines = [|"..##.......";
                      "#...#...#..";
                      ".#....#..#.";
                      "..#.#...#.#";
                      ".#...##..#.";
                      "..#.##.....";
                      ".#.#.#....#";
                      ".#........#";
                      "#.##...#...";
                      "#...##....#";
                      ".#..#...#.#"|]
        }
        let result = DayThree.solve puzzle
        Assert.Equal("336", result)

