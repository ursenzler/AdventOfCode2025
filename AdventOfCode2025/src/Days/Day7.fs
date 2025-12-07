module Days.Day7

open System.Collections.Generic
open System.IO
open FSharpx

let solve1 path =
    let findStartPosition manifold =
        manifold |> List.head |> String.indexOfChar 'S' |> List.singleton, manifold |> List.skip 1

    let printManifold (beams: int list) (manifold: string list) =
        manifold
        |> List.iteri (fun l line ->
            line
            |> Seq.iteri (fun i c ->
                if l = 0 && beams |> List.contains i then
                    printf "|"
                else
                    printf $"{c}")

            printfn "")

        printfn "------------------"

    let rec runBeams sum beams (manifold: string list) =
        match manifold with
        | [] -> sum
        | top :: rest ->
            let splitters = beams |> List.filter (fun beam -> top[beam] = '^')

            let newBeams =
                beams
                |> List.collect (fun beam ->
                    match top[beam] with
                    | '^' -> [ beam - 1; beam + 1 ]
                    | '.'
                    | _ -> [ beam ])
                |> List.distinct

            // printfn $"Sum: {sum} Splitters: {splitters.Length}"
            // printManifold newBeams manifold
            
            runBeams (sum + splitters.Length) newBeams rest
            
    path
    |> File.ReadAllLines
    |> Array.toList
    |> findStartPosition
    |> fun (beams, manifold) -> runBeams 0 beams manifold


let solve2 path =
    let paths = Dictionary<int * int, int64> []

    let rec runBeam beam line (manifold: string list) =
        match paths.TryGetValue((beam, line)) with
        | true, value -> value
        | false, _ ->
            let numberOfTimelines =
                match manifold with
                | [] -> 1L
                | top :: rest ->
                    match top[beam] with
                    | '^' -> runBeam (beam - 1) (line + 1) rest + runBeam (beam + 1) (line + 1) rest
                    | '.' | _ -> runBeam beam (line + 1) rest
            paths.Add((beam, line), numberOfTimelines)
            numberOfTimelines
           
    let findSingleStartPosition manifold =
        manifold |> List.head |> String.indexOfChar 'S', manifold |> List.skip 1
        
    path
    |> File.ReadAllLines
    |> Array.toList
    |> findSingleStartPosition
    |> fun (beam, manifold) -> runBeam beam 0 manifold


let solve2' path =
    let rec runBeam (paths: Map<int * int, int64>) beam line (manifold: string list): int64 * Map<int * int, int64> =
        match paths |> Map.tryFind (beam, line) with
        | Some value -> value, paths
        | None ->
            match manifold with
            | [] -> 1L, paths
            | top :: rest ->
                match top[beam] with
                | '^' ->
                    let left, paths = runBeam paths (beam - 1) (line + 1) rest
                    let paths = paths |> Map.add (beam - 1, line + 1) left
                    let (right: int64, paths) = runBeam paths (beam + 1) (line + 1) rest
                    let paths = paths |> Map.add (beam + 1, line + 1) right
                    left + right, paths
                | '.' | _ ->
                    runBeam paths beam (line + 1) rest
           
    let findStartPosition manifold =
        manifold |> List.head |> String.indexOfChar 'S', manifold |> List.skip 1
    
    path
    |> File.ReadAllLines
    |> Array.toList
    |> findStartPosition
    |> fun (beam, manifold) -> runBeam Map.empty beam 0 manifold
    |> fst

let solve () =
    let pathInput = Path.Combine(__SOURCE_DIRECTORY__, "Day7Input.txt")
    printfn $"""Total hit splitters = %A{solve1 pathInput}"""
    printfn $"""Total timelines = %A{solve2 pathInput}"""
    printfn $"""Total timelines = %A{solve2' pathInput}"""
