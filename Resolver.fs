module adventofcode20.Resolver

module DayOne =
    open CommonTypes

    let calculateFuel (mass: int) =
        mass ./. 3 |> floor |> int |> fun x -> x - 2

    let solve puzzle =
        puzzle.Lines
        |> Array.map (fun x -> x |> int |> calculateFuel)
        |> Array.sum
        |> string
