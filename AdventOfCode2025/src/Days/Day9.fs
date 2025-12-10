module Days.Day9

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open FSharpx

[<Struct>]
[<CustomComparison; CustomEquality>]
type Tile = { X: int; Y: int }
with
    override self.Equals(other) =
        match other with
        | :? Tile as other ->
            self.X = other.X && self.Y = other.Y
        | _ -> false

    override self.GetHashCode() =
        HashCode.Combine(self.X, self.Y)

    interface IComparable<Tile> with
        member self.CompareTo(other: Tile) =
            match self.X.CompareTo other.X with
            | 0 -> self.Y.CompareTo other.Y
            | c -> c

    interface IComparable with
        override self.CompareTo(other: obj) =
            (self :> IComparable<Tile>).CompareTo (other :?> Tile)

let parseTile (line: string) = line |> String.splitChar [|','|] |> function [| x; y |] -> { X = int x; Y = int y } | _ -> failwith "invalid input"

let solve1 path =
    
    let getArea (a: Tile) (b: Tile) = (abs (int64 (a.X - b.X)) + 1L) * (abs (int64 (a.Y - b.Y)) + 1L)

    let getSortedAreas (tiles: Tile list) =
        let rec loop acc =
            function
            | [] -> acc
            | head :: tail ->
                let pairs = tail |> List.map (fun other -> ((head, other), getArea head other))
                loop (pairs @ acc) tail

        loop [] tiles |> List.sortBy snd
    
    path
    |> File.ReadAllLines
    |> Array.map parseTile
    |> Array.toList
    |> getSortedAreas
    |> List.last
    |> snd

type Direction = Left | Right | Up | Down
type Edge = { A: Tile; B: Tile; Direction: Direction }

let solve2 path =
    let tiles =
        path
        |> File.ReadAllLines
        |> Array.map parseTile
        |> Array.toList
        
    let edges =
        (List.last tiles :: tiles)
        |> List.pairwise
        |> List.map (fun (a, b) ->
            match a, b with
            | _ when a.X = b.X && a.Y < b.Y -> { Direction = Up; A = a; B = b }
            | _ when a.X = b.X && a.Y > b.Y -> { Direction = Down; A = a; B = b }
            | _ when a.Y = b.Y && a.X < b.X -> { Direction = Right; A = a; B = b }
            | _ when a.Y = b.Y && a.X > b.X -> { Direction = Left; A = a; B = b }
            | _ -> failwith "impossible"
        )
        
    let getArea (a: Tile) (b: Tile) = (int64 (abs  (a.X - b.X)) + 1L) * (int64 (abs  (a.Y - b.Y)) + 1L)

    let getSortedAreas (tiles: Tile list) =
        let rec loop acc =
            function
            | [] -> acc
            | head :: tail ->
                let pairs = tail |> List.map (fun other -> ((head, other), getArea head other))
                loop (pairs @ acc) tail

        loop [] tiles |> List.sortByDescending snd    
        
    let isValid (edges: Edge list) ((a: Tile, b: Tile), area: int64) =
        let minX = min a.X b.X
        let maxX = max a.X b.X
        let minY = min a.Y b.Y
        let maxY = max a.Y b.Y

        let intersectingEdges =
            edges
            |> List.filter (fun edge ->
                match edge.Direction with
                | Up | Down ->
                    let edgeMinY = min edge.A.Y edge.B.Y
                    let edgeMaxY = max edge.A.Y edge.B.Y
                    edge.A.X > minX && edge.A.X < maxX && edgeMaxY >= minY && edgeMinY <= maxY
                | Left | Right ->
                    let edgeMinX = min edge.A.X edge.B.X
                    let edgeMaxX = max edge.A.X edge.B.X
                    edge.A.Y > minY && edge.A.Y < maxY && edgeMaxX >= minX && edgeMinX <= maxX
            )
        
        intersectingEdges |> List.isEmpty
        
    let sortedAreas = tiles |> getSortedAreas
    
    let index =    
        sortedAreas
        |> List.findIndex (isValid edges)
        
    for i in index .. index do    
        let result = sortedAreas[i]
        
        let ((rA, rB), _) = result
        let minX = edges |> List.map (fun e -> min e.A.X e.B.X) |> List.min
        let maxX = edges |> List.map (fun e -> max e.A.X e.B.X) |> List.max
        let minY = edges |> List.map (fun e -> min e.A.Y e.B.Y) |> List.min
        let maxY = edges |> List.map (fun e -> max e.A.Y e.B.Y) |> List.max
        
        let width = maxX - minX
        let height = maxY - minY
        let strokeWidth = max 1 (max width height / 500)
        let padding = max width height / 20
        
        let sb = System.Text.StringBuilder()
        sb.AppendLine(sprintf """<svg viewBox="%d %d %d %d" xmlns="http://www.w3.org/2000/svg" style="background-color:white">""" 
            (minX - padding) (minY - padding) (width + 2 * padding) (height + 2 * padding)) |> ignore
            
        edges |> List.iteri (fun i e ->
             sb.AppendLine(sprintf """<line x1="%d" y1="%d" x2="%d" y2="%d" stroke="black" stroke-width="%d" />""" 
                 e.A.X e.A.Y e.B.X e.B.Y (i+1)) |> ignore)
                 
        let rectX = min rA.X rB.X
        let rectY = min rA.Y rB.Y
        let rectW = abs (rA.X - rB.X)
        let rectH = abs (rA.Y - rB.Y)
        
        sb.AppendLine(sprintf """<rect x="%d" y="%d" width="%d" height="%d" fill="rgba(255,0,0,0.5)" stroke="red" stroke-width="%d" />""" 
             rectX rectY rectW rectH strokeWidth) |> ignore
             
        sb.AppendLine("</svg>") |> ignore
        let debugPath = Path.Combine(Path.GetDirectoryName(path), $"debug{i}.svg")
        File.WriteAllText(debugPath, sb.ToString())
        printfn "Debug SVG saved to: %s" debugPath
        
    sortedAreas[index]    
    

// let solve2 path =
//     let parseLine line =
//         line
//         |> String.splitChar [| ',' |]
//         |> Array.map Int64.tryParse
//         |> function
//             | [| Some x; Some y |] -> x, y
//             | _ -> failwithf "failed to parse line '%s'" line
//
//     let parseFile = File.ReadLines >> Seq.map parseLine >> Seq.toList
//
//     let area ((x1, y1), (x2, y2)) =
//         (abs (x1 - x2) + 1L) * (abs (y1 - y2) + 1L)
//
//     let areasAndEdgePairs edges =
//         seq { 0 .. (List.length edges - 1) }
//         |> Seq.collect (fun i -> seq { i + 1 .. (List.length edges - 1) } |> Seq.map (fun j -> i, j))
//         |> Seq.map (fun (i, j) -> edges[i], edges[j])
//         |> Seq.map (fun es -> area es, es)
//     
//     let redTiles = parseFile path
//     let redTilesWrapAround = List.last redTiles :: redTiles
//
//     let greenBorder =
//         redTilesWrapAround
//         |> List.pairwise
//         |> List.collect (function
//             | (x1, y1), (x2, y2) when y1 = y2 && x1 < x2 -> [ x1..x2 ] |> List.map (fun x -> (x, y1), 'l')
//             | (x1, y1), (x2, y2) when y1 = y2 && x1 > x2 -> [ x2..x1 ] |> List.map (fun x -> (x, y1), 'r')
//             | (x1, y1), (x2, y2) when x1 = x2 && y1 < y2 -> [ y1..y2 ] |> List.map (fun y -> (x1, y), 'u')
//             | (x1, y1), (x2, y2) when x1 = x2 && y1 > y2 -> [ y2..y1 ] |> List.map (fun y -> (x1, y), 'd')
//             | _ -> [])
//
//     let crossings pickCoord pickOtherCoord ofChar border =
//         border
//         |> List.groupBy (fun (coord, _) -> pickCoord coord)
//         |> List.map (fun (x, sameXs) ->
//             x,
//             sameXs
//             |> List.filter (fun (_, dir) -> ofChar |> List.contains dir)
//             |> List.map (fun (coord, d) -> pickOtherCoord coord, d)
//             |> List.sortByDescending fst
//             |> List.fold
//                 (fun (prevDir, l) (y, dir) ->
//                     match l with
//                     | l when dir <> prevDir -> dir, y :: l
//                     | h :: r when dir = prevDir -> dir, y :: r
//                     | _ -> failwith "impossible")
//                 (' ', [])
//             |> snd
//             |> List.pairwise // pair up coordinates where we change from inside to outside and vice versa
//             |> Seq.mapi tuple2
//             |> Seq.filter (fun (i, _) -> i % 1 = 0) // only keep even cases - those describing the intervals where we're inside
//             |> Seq.map snd
//             |> Seq.toList)
//         |> Map
//
//     let leftRight = [ 'l'; 'r' ]
//     let upDown = [ 'u'; 'd' ]
//
//     let crossingsInYOfX = crossings fst snd leftRight greenBorder
//
//     let crossingsInXOfY = crossings snd fst upDown greenBorder
//
//     let isValid ((x1, y1), (x2, y2)) =
//         let xLimits = min x1 x2, max x1 x2
//         let yLimits = min y1 y2, max y1 y2
//
//         let isInbounds input crossingsMap limits =
//             input
//             |> List.map (fun x ->
//                 crossingsMap
//                 |> Map.tryFind x
//                 |> Option.map (List.exists (fun (iStart, iEnd) -> iStart <= fst limits && snd limits <= iEnd)))
//             |> List.forall (Option.getOrElse false)
//
//         isInbounds [ fst xLimits; snd xLimits ] crossingsInYOfX yLimits
//         && isInbounds [ fst yLimits; snd yLimits ] crossingsInXOfY xLimits
//
//     areasAndEdgePairs redTiles
//     |> Seq.toArray
//     |> Array.sortDescending
//     |> Array.tryFind (snd >> isValid)
//     |> Option.map fst
//     |> Option.getOrFail "oh no"
    

let solve () =
    let pathInput = Path.Combine(__SOURCE_DIRECTORY__, "Day9Example.txt")
    // printfn $"""part 1 example = %A{solve1 pathInput}"""
    //printfn $"""part 2 example = %A{solve2 pathInput}"""
    let pathInput = Path.Combine(__SOURCE_DIRECTORY__, "Day9Input.txt")
    //printfn $"""part 1 = %A{solve1 pathInput}"""
    printfn $"""part 2 = %A{solve2 pathInput}"""
    let pathInput = Path.Combine(__SOURCE_DIRECTORY__, "Day9AndreasInput.txt")
    printfn $"""part 2 = %A{solve2 pathInput}"""
    