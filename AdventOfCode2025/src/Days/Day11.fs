module Days.Day11

open System
open System.Collections.Concurrent
open System.IO
open FSharpx

type Device = { Name: string; Connections: string list }

let parseDevice (line: string) =
    let parts = line |> String.splitCharWithOptions [| ':'; ' ' |] StringSplitOptions.RemoveEmptyEntries
    {
        Name = parts[0]
        Connections = parts[1..] |> Array.toList
    }

let parseDevices path = path |> File.ReadAllLines |> Array.map parseDevice |> Array.toList

let solve1 path =

    let rec findAllPathsToOut devices device =
        let current =
            devices
            |> List.tryFind (fun d -> d.Name = device)
            |> Option.getOrFail $"could not find device {device}"

        match current.Connections with
        | [] -> 0
        | [ "out" ] -> 1
        | connections -> connections |> List.sumBy (findAllPathsToOut devices)

    let devices = path |> parseDevices

    findAllPathsToOut devices "you"


let solve2 path =
    let devices = path |> parseDevices

    let mutable memo = Map.empty

    let rec findAllPathsToOut (visited: Set<string>) devices device =
        let current =
            devices
            |> List.tryFind (fun d -> d.Name = device)
            |> Option.getOrFail $"could not find device {device}"

        let dacVisited = visited |> Set.contains "dac"
        let fftVisited = visited |> Set.contains "fft"

        match memo |> Map.tryFind (current.Name, dacVisited, fftVisited) with
        | Some r -> r
        | None ->
            let result =
                if visited.Contains current.Name then
                    0L
                else
                    match current.Connections with
                    | [] -> 0
                    | [ "out" ] when dacVisited && fftVisited -> 1
                    | [ "out" ] -> 0L
                    | connections ->
                        connections |> List.sumBy (findAllPathsToOut (visited |> Set.add current.Name) devices)

            memo <- memo |> Map.add (current.Name, dacVisited, fftVisited) result

            result

    findAllPathsToOut Set.empty devices "svr"
    
    
let solve2parallel path =
    let devices = path |> parseDevices

    let mutable memo = ConcurrentDictionary<string * bool * bool, int64>()

    let rec findAllPathsToOut (visited: Set<string>) devices device =
        let current =
            devices
            |> List.tryFind (fun d -> d.Name = device)
            |> Option.getOrFail $"could not find device {device}"

        let dacVisited = visited |> Set.contains "dac"
        let fftVisited = visited |> Set.contains "fft"

        match memo.TryGetValue((current.Name, dacVisited, fftVisited)) with
        | true,  r -> r
        | false, _ ->
            let result =
                if visited.Contains current.Name then
                    0L
                else
                    match current.Connections with
                    | [] -> 0
                    | [ "out" ] when dacVisited && fftVisited -> 1
                    | [ "out" ] -> 0L
                    | connections ->
                        connections
                        |> List.toArray
                        |> Array.map (fun connection -> fun () -> findAllPathsToOut (visited |> Set.add current.Name) devices connection)
                        |> Array.Parallel.sumBy (fun f -> f())

            memo.TryAdd((current.Name, dacVisited, fftVisited), result) |> ignore

            result

    findAllPathsToOut Set.empty devices "svr"    


let solve () =
    // let pathInput = Path.Combine(__SOURCE_DIRECTORY__, "Day11Example1.txt")
    // printfn $"""part 1 example = %A{solve1 pathInput}"""
    let pathInput = Path.Combine (__SOURCE_DIRECTORY__, "Day11Example2.txt")
    //printfn $"""part 2 example = %A{solve2 pathInput}"""
    let pathInput = Path.Combine (__SOURCE_DIRECTORY__, "Day11Input.txt")
    //printfn $"""part 1 = %A{solve1 pathInput}"""
    
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    
    let solve2 = solve2 pathInput
    stopwatch.Stop()
    printfn $"""part 2 = %A{solve2} in %A{stopwatch.ElapsedMilliseconds}ms"""
    
    stopwatch.Restart()
    let solve2Parallel = solve2parallel pathInput
    stopwatch.Stop()
    printfn $"""part 2 parallel = %A{solve2Parallel} in %A{stopwatch.ElapsedMilliseconds}ms"""
