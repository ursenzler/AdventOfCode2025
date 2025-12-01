namespace Days

open System.IO
open System

module Int =
    let tryParse (value: string) =
        match Int32.TryParse value with
        | true, num -> Some num
        | _ -> None

module Day1 =
    let wrapAround = 100
    let startPos = 50

    let parseLine =
        function
        | str when str |> String.length > 1 ->
            match str[0], Int.tryParse str[1..] with
            | 'L', Some steps -> Some -steps
            | 'R', Some steps -> Some steps
            | _ -> None
        | _ -> None

    let wrapAroundPosition x =
        let rem = x % wrapAround
        rem + if rem < 0 then wrapAround else 0

    let parseFile = File.ReadLines >> Seq.toList >> List.choose parseLine

    let executeSteps =
        List.scan (fun pos steps -> pos + steps |> wrapAroundPosition) startPos

    let countZerosVisited = List.sumBy (fun x -> if x = 0 then 1 else 0)

    let solve1 path =
        path |> parseFile |> executeSteps |> countZerosVisited

    let expandToSingleSteps = List.collect (fun i -> List.replicate (abs i) (sign i))

    let solve2 path =
        path |> parseFile |> expandToSingleSteps |> executeSteps |> countZerosVisited

    let solve () =
        let pathInput1 = Path.Combine(__SOURCE_DIRECTORY__, "Day1Input1.txt")

        let result1 = solve1 pathInput1
        let result2 = solve2 pathInput1

        printfn $"Day 1, Part 1: {result1}"
        printfn $"Day 1, Part 2: {result2}"
