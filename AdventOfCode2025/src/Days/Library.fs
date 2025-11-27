namespace Days
open System.IO
open System

module Day1 =
    let input file = File.ReadAllLines file
    let parseLine (line : string) = 
        line.Split ' ' 
        |> Array.filter (String.IsNullOrEmpty >> not) 
        |> Array.map Int32.Parse
        |> fun pair -> (pair[0], pair[1])
    let parseFile file = 
        input file 
        |> Array.map parseLine

    let solveImpl1 file =
        parseFile file
        |> Array.unzip
        |> fun (x, y) -> Array.sort x, Array.sort y
        ||> Array.zip
        |> Array.sumBy (fun (a,b) -> Math.Abs(a-b))

    let solveImpl2 file =
        parseFile file
        |> Array.unzip
        |> fun (xs, ys) -> 
            let freqMap = 
                ys 
                |> Array.groupBy id 
                |> Array.map (fun (k, arr) -> k, arr.Length) 
                |> dict
            xs |> Array.sumBy (fun x ->
                let n = if freqMap.ContainsKey(x) then freqMap.[x] else 0
                x * n)

    let solve =
        let result1 = solveImpl1 "../Days/data/day01_1.txt"
        let result2 = solveImpl2 "../Days/data/day01_1.txt"
        printfn "%d ref(3574690)" result1
        printfn "%d ref(22565391)" result2
