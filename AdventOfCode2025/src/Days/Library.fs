namespace Days

open System.IO
open System

module Day1 =
    let readFile file = File.ReadAllLines file |> Array.toList

    let parseLine (line: string) =
        line.Split ' '
        |> Array.toList
        |> List.choose (fun (str: string) ->
            match Int32.TryParse str with
            | true, num -> Some num
            | _ -> None)
        |> fun xs ->
            match xs with
            | elem1 :: elem2 :: [] -> Some(elem1, elem2)
            | _ -> None

    // parseFile to two lists of the values in the file *in reverse order* 
    let parseFile file =
        readFile file
        |> List.fold
            (fun (accX, accY) line ->
                match parseLine line with
                | Some(x, y) -> x :: accX, y :: accY
                | None -> accX, accY)
            ([], [])

    let solveImpl1 file =
        let absDiff : (int * int) -> int = (fun (x, y) -> Math.Abs(x - y))
        parseFile file
        |> fun (xs, ys) -> List.zip (List.sort xs) (List.sort ys)
        |> List.sumBy absDiff

    let solveImpl2 file =
        let freqMap nums =
            nums
            |> List.groupBy id
            |> List.map (fun (k, arr) -> k, arr.Length)
            |> Map.ofList

        parseFile file
        |> fun (xs, ys) ->
            let xsFreqMap = xs |> freqMap
            let ysFreqMap = ys |> freqMap

            xsFreqMap
            |> Map.fold
                (fun acc x xcount ->
                    let ycount = Map.tryFind x ysFreqMap |> Option.defaultValue 0
                    acc + x * xcount * ycount)
                0

    let solve =
        let result1 = solveImpl1 "../Days/data/day01_1.txt"
        let result2 = solveImpl2 "../Days/data/day01_1.txt"
        printfn "%d ref(3574690)" result1
        printfn "%d ref(22565391)" result2
    