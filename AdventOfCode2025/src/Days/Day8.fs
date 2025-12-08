module Days.Day8

open System.IO
open FSharpx

// let's use a struct which is hopefully faster than a record
[<Struct>]
type Point = { X: int; Y: int; Z: int }

// type alias so I'm not confused by list of lists
type Cluster = Point list

let parseCoordinate line =
    line
    |> String.splitChar [| ',' |]
    |> Array.choose Int.tryParse
    |> function
        | [| x; y; z |] -> { X = x; Y = y; Z = z }
        | _ -> failwith "Invalid coordinate"

let getDistance p1 p2 =
    let dx = double (p1.X - p2.X)
    let dy = double (p1.Y - p2.Y)
    let dz = double (p1.Z - p2.Z)
    sqrt (dx * dx + dy * dy + dz * dz)

let getSortedDistances (points: Point list) =
    let rec loop acc =
        function
        | [] -> acc
        | head :: tail ->
            let pairs = tail |> List.map (fun other -> ((head, other), getDistance head other))
            loop (pairs @ acc) tail

    loop [] points |> List.sortBy snd

let getCluster p = List.find (fun cluster -> cluster |> List.contains p)

let solve1 iterations path =
    let boxes = path |> File.ReadAllLines |> Array.map parseCoordinate |> Array.toList
    let sortedDistances = boxes |> getSortedDistances |> List.take iterations

    sortedDistances
    |> List.fold
        (fun (clusters: Cluster list) ((a, b), _) ->
            let clusterA = clusters |> getCluster a
            let clusterB = clusters |> getCluster b

            if (clusterA <> clusterB) then
                (clusterA @ clusterB)
                :: (clusters
                    |> List.filter (fun cluster -> cluster <> clusterA && cluster <> clusterB))
            else
                clusters)
        (boxes |> List.map List.singleton)
    |> List.sortByDescending _.Length
    |> List.take 3
    |> List.map _.Length
    |> List.reduce (*)

let solve2 path =
    let boxes = path |> File.ReadAllLines |> Array.map parseCoordinate |> Array.toList
    let sortedDistances = boxes |> getSortedDistances

    sortedDistances
    |> List.fold
        (fun (clusters: Cluster list, latestPair) ((a, b), _) ->
            if clusters.Length = 1 then
                clusters, latestPair
            else
                let clusterA = clusters |> getCluster a
                let clusterB = clusters |> getCluster b

                if (clusterA <> clusterB) then
                    ((clusterA @ clusterB)
                     :: (clusters
                         |> List.filter (fun cluster -> cluster <> clusterA && cluster <> clusterB)),
                     Some(a, b))
                else
                    clusters, latestPair)
        (boxes |> List.map List.singleton, None)
    |> snd
    |> Option.get
    |> fun (a, b) -> a.X * b.X

let solve () =
    let pathInput = Path.Combine(__SOURCE_DIRECTORY__, "Day8Example.txt")
    printfn $"""part 1 example = %A{solve1 10 pathInput}"""
    let pathInput = Path.Combine(__SOURCE_DIRECTORY__, "Day8Input.txt")
    printfn $"""part 1 = %A{solve1 1000 pathInput}"""
    printfn $"""part 2 = %A{solve2 pathInput}"""
