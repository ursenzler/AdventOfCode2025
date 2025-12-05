module Days.Day5

open System
open System.IO
open FSharpx

let splitIntoFreshRangesAndAvailableIngredients lines =
    let breakIndex = lines |> Array.findIndex String.IsNullOrWhiteSpace
    lines[0 .. breakIndex - 1], lines[breakIndex + 1 ..]

let parseFreshRange line =
    match line |> String.splitChar [| '-' |] |> Array.toList with
    | [ from; until ] -> from |> int64, until |> int64
    | _ -> failwith $"Invalid fresh range {line}"

let isFresh freshRanges ingredient =
    freshRanges
    |> Array.exists (fun (from, until) -> from <= ingredient && ingredient <= until)

let solve1 path =
    let freshLines, availableLines =
        path |> File.ReadAllLines |> splitIntoFreshRangesAndAvailableIngredients

    let fresh = freshLines |> Array.map parseFreshRange
    let available = availableLines |> Array.map int64

    available |> Array.filter (isFresh fresh) |> Array.length

let intersectRanges ranges =
    ranges
    |> Array.sortBy fst
    |> Array.fold
        (fun acc (start, finish) ->
            match acc with
            | [] -> [ (start, finish) ]
            | (lastStart, lastFinish) :: tail ->
                if start <= lastFinish + 1L then
                    (lastStart, max lastFinish finish) :: tail
                else
                    (start, finish) :: acc)
        []
    |> List.toArray

let countIngredients ranges =
    ranges |> Array.map (fun (start, finish) -> finish - start + 1L) |> Array.sum

let solve2 path =
    let freshLines, _ =
        path |> File.ReadAllLines |> splitIntoFreshRangesAndAvailableIngredients

    freshLines |> Array.map parseFreshRange |> intersectRanges |> countIngredients

let solve () =
    let pathInput = Path.Combine(__SOURCE_DIRECTORY__, "Day5Input1.txt")
    printfn $"""%A{solve1 pathInput}"""
    printfn $"""%A{solve2 pathInput}"""
