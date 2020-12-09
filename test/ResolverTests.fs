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

    [<Fact>]
    let ``solve part two invalid``() =
        let puzzle = {
            Day = 4
            Part = 2
            Lines = [|  "eyr:1972 cid:100"
                        "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
                        ""
                        "iyr:2019"
                        "hcl:#602927 eyr:1967 hgt:170cm"
                        "ecl:grn pid:012533040 byr:1946"
                        ""
                        "hcl:dab227 iyr:2012"
                        "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
                        ""
                        "hgt:59cm ecl:zzz"
                        "eyr:2038 hcl:74454a iyr:2023"
                        "pid:3556412378 byr:2007"|]
        }
        let result = DayFour.solve puzzle
        Assert.Equal("0", result)

    [<Fact>]
    let ``solve part two valid``() =
        let puzzle = {
            Day = 4
            Part = 2
            Lines = [|  "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
                        "hcl:#623a2f"
                        ""
                        "eyr:2029 ecl:blu cid:129 byr:1989"
                        "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
                        ""
                        "hcl:#888785"
                        "hgt:164cm byr:2001 iyr:2015 cid:88"
                        "pid:545766238 ecl:hzl"
                        "eyr:2022"
                        ""
                        "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719" |]
        }
        let result = DayFour.solve puzzle
        Assert.Equal("4", result)

module DayFive =

    [<Fact>]
    let ``solve example part one``() =
        let puzzle = {
            Day = 5
            Part = 1
            Lines = [|  "BFFFBBFRRR"
                        "FFFBBBFRRR"
                        "BBFFBBFRLL"|]
        }
        let result = DayFive.solve puzzle
        Assert.Equal("820", result)

module DaySix =

    [<Fact>]
    let ``solve example part one``() =
        let puzzle = {
            Day = 6
            Part = 1
            Lines = [|  "abc"
                        ""
                        "a"
                        "b"
                        "c"
                        ""
                        "ab"
                        "ac"
                        ""
                        "a"
                        "a"
                        "a"
                        "a"
                        ""
                        "b"|]
        }
        let result = DaySix.solve puzzle
        Assert.Equal("11", result)

    [<Fact>]
    let ``solve example part two``() =
        let puzzle = {
            Day = 6
            Part = 2
            Lines = [|  "abc"
                        ""
                        "a"
                        "b"
                        "c"
                        ""
                        "ab"
                        "ac"
                        ""
                        "a"
                        "a"
                        "a"
                        "a"
                        ""
                        "b"|]
        }
        let result = DaySix.solve puzzle
        Assert.Equal("6", result)

module DaySeven =

    [<Fact>]
    let ``solve example part one``() =
        let puzzle = {
            Day = 7
            Part = 1
            Lines = [|  "light red bags contain 1 bright white bag, 2 muted yellow bags."
                        "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
                        "bright white bags contain 1 shiny gold bag."
                        "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
                        "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
                        "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
                        "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
                        "faded blue bags contain no other bags."
                        "dotted black bags contain no other bags."|]
        }
        let result = DaySeven.solve puzzle
        Assert.Equal("4", result)

    [<Fact>]
    let ``solve example part two``() =
        let puzzle = {
            Day = 7
            Part = 2
            Lines = [|  "shiny gold bags contain 2 dark red bags."
                        "dark red bags contain 2 dark orange bags."
                        "dark orange bags contain 2 dark yellow bags."
                        "dark yellow bags contain 2 dark green bags."
                        "dark green bags contain 2 dark blue bags."
                        "dark blue bags contain 2 dark violet bags."
                        "dark violet bags contain no other bags."|]
        }
        let result = DaySeven.solve puzzle
        Assert.Equal("126", result)

module DayEight =

    [<Fact>]
    let ``solve example part one``() =
        let puzzle = {
            Day = 8
            Part = 1
            Lines = [|  "nop +0"
                        "acc +1"
                        "jmp +4"
                        "acc +3"
                        "jmp -3"
                        "acc -99"
                        "acc +1"
                        "jmp -4"
                        "acc +6" |]
        }
        let result = DayEight.solve puzzle
        Assert.Equal("5", result)

    [<Fact>]
    let ``solve example part two``() =
        let puzzle = {
            Day = 8
            Part = 2
            Lines = [|  "nop +0"
                        "acc +1"
                        "jmp +4"
                        "acc +3"
                        "jmp -3"
                        "acc -99"
                        "acc +1"
                        "jmp -4"
                        "acc +6" |]
        }
        let result = DayEight.solve puzzle
        Assert.Equal("8", result)

module DayNine =

    [<Fact>]
    let ``solve example part one``() =
        let puzzle = {
            Day = 9
            Part = 1
            Lines = [|  "35"
                        "20"
                        "15"
                        "25"
                        "47"
                        "40"
                        "62"
                        "55"
                        "65"
                        "95"
                        "102"
                        "117"
                        "150"
                        "182"
                        "127"
                        "219"
                        "299"
                        "277"
                        "309"
                        "576" |]
        }
        let result = DayNine.solve puzzle
        Assert.Equal("127", result)
