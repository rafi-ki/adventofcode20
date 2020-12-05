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

module DayFour =

    [<Fact>]
    let ``solve example part one``() =
        let puzzle = {
            Day = 4
            Part = 1
            Lines = [|  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd";
                        "byr:1937 iyr:2017 cid:147 hgt:183cm";
                        "";
                        "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884";
                        "hcl:#cfa07d byr:1929";
                        "";
                        "hcl:#ae17e1 iyr:2013";
                        "eyr:2024";
                        "ecl:brn pid:760753108 byr:1931";
                        "hgt:179cm";
                        "";
                        "hcl:#cfa07d eyr:2025 pid:166559648";
                        "iyr:2011 ecl:brn hgt:59in"|]
        }
        let result = DayFour.solve puzzle
        Assert.Equal("2", result)
