module Days.Day3

open System
open System.Diagnostics
open System.IO
open FSharpx

// brute force part 1
let findCandidates1 (line: string) =
    seq {
        for i in 0 .. line.Length - 2 do
            for j in i + 1 .. line.Length - 1 do
                let v = $"{line[i]}{line[j]}" |> Int64.tryParse |> Option.get
                v
    }

let findLargestPossibleJoltage (findCandidates: string -> int64 seq) (line: string) =
    printfn ""
    printfn $"""{line}"""

    line |> findCandidates |> Seq.max

let solve1 findCandidates path =
    File.ReadAllLines path
    |> Seq.map (findLargestPossibleJoltage findCandidates)
    |> Seq.sum

// clever solution inspired by Andreas for part 2

let parseLine line =
    line
    |> String.toCharArray
    |> Array.choose (string >> Int32.parse)

let getIndexOfEarliestMax (xs: array<int>) =
    xs |> Array.indexed |> Array.maxBy snd |> fst

[<TailCall>]
let rec maxJoltageBatteries pick (nums: int[]) =
    match pick with
    | 0 -> []
    | _ ->
        let pickableBatteries =  nums[0..Array.length nums - pick]
        let ind = getIndexOfEarliestMax pickableBatteries

        int64 pickableBatteries[ind]
        :: maxJoltageBatteries (pick - 1) (Array.skip (ind + 1) nums)

let maxJoltage pick nums =
    nums |> maxJoltageBatteries pick |> List.reduce (fun a b -> 10L * a + b)

let solve2 path  =
    path
    |> File.ReadAllLines
    |> Array.map parseLine 
    |> Seq.sumBy (maxJoltage 12)

let solve () =
    let pathInput1 = Path.Combine(__SOURCE_DIRECTORY__, "Day3Input1.txt")
    printfn $"""%A{solve1 findCandidates1 pathInput1}"""
    printfn $"""%A{solve2 pathInput1}"""
