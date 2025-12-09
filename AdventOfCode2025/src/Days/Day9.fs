module Days.Day9

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open FSharpx

[<Struct>]
[<CustomComparison; CustomEquality>]
type Tile = { X: int64; Y: int64 }
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
    
    let getArea (a: Tile) (b: Tile) = (abs (a.X - b.X) + 1L) * (abs (a.Y - b.Y) + 1L)

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


let solve2 path =
    
    let getArea (a: Tile) (b: Tile) = (abs (a.X - b.X) + 1L) * (abs (a.Y - b.Y) + 1L)
    
    let getAllTilesInArea (a: Tile) (b: Tile) =
        [
            for x in [ min a.X b.X .. max a.X b.X ] do
                for y in [ min a.Y b.Y .. max a.Y b.Y ] do
                    { X = x; Y = y }
        ]
        
    let getSortedAreas (tiles: Tile list) =
        let rec loop acc =
            function
            | [] -> acc
            | head :: tail ->
                let pairs = tail |> List.map (fun other -> ((head, other), getArea head other))
                loop (pairs @ acc) tail

        loop [] tiles |> List.sortByDescending snd    
        
    let getTilesFromTo (a: Tile) (b: Tile) =
        if a.X = b.X then [ for y in [ min a.Y b.Y .. max a.Y b.Y ] -> { X = a.X; Y = y } ]
        else [ for x in [ min a.X b.X .. max a.X b.X ] -> { X = x; Y = a.Y } ]
       
    let getBorderTiles (tiles: Tile list) =
        (tiles @ [ tiles.Head ])
        |> List.pairwise
        |> List.collect (fun (a, b) -> getTilesFromTo a b)
        
    let findInsideTiles (border: Tile list) =
        let minX = border |> List.minBy _.X |> _.X
        let maxX = border |> List.maxBy _.X |> _.X
        let minY = border |> List.minBy _.Y |> _.Y
        let maxY = border |> List.maxBy _.Y |> _.Y
        let border = HashSet border
        
        // let's find all the tiles outside
        let outside = HashSet<Tile>()
        
        // queue with the whole border (we add one tile around so they are all not green or red)
        let q = Queue<Tile>()
        [
            // top and bottom border
            for x in [ minX - 1L .. maxX + 1L ] do
                for y in [ minY - 1L; maxY + 1L ] do
                    { X = x; Y = y }
            // left and right border        
            for x in [ minX - 1L; maxX + 1L ] do
                for y in [ minY - 1L .. maxY + 1L ] do
                    { X = x; Y = y }
        ]
        |> List.iter q.Enqueue
        
        let directions = [| (0L, 1L); (0L, -1L); (1L, 0L); (-1L, 0L) |]
         
        let stopwatch = Stopwatch()
        stopwatch.Start()
         
        while q.Count > 0 do
             if stopwatch.ElapsedMilliseconds > 1000L then
                printfn "queue size: %d" q.Count
                stopwatch.Restart()
                
             let current = q.Dequeue()
             
             // printfn "---------------"
             // for l in 0 .. 8 do
             //    for c in 0 .. 13 do
             //        if current = { X = int64 c; Y = int64 l } then printf "0"
             //        else if border.Contains { X = int64 c; Y = int64 l } then printf "#"
             //        else if outside.Contains { X = int64 c; Y = int64 l } then printf "_"
             //        else printf "."
             //    printfn ""
             // printfn "---------------"   
             
             for dx, dy in directions do
                let nx, ny = current.X + dx, current.Y + dy
                let next = { X = nx; Y = ny }
                
                let inBounds = nx >= minX && nx <= maxX && ny >= minY && ny <= maxY
                
                if inBounds && not (outside.Contains next) && not (border.Contains next) then
                    outside.Add next |> ignore
                    q.Enqueue next

        [
            for y in minY .. maxY do
                for x in minX .. maxX do
                    let t = { X = x; Y = y }
                    if not (outside.Contains t) then
                        t
        ]
    
    let hasOnlyRedOrGreenTiles (redOrGreenTiles: Tile list) (a: Tile) (b: Tile) =
        let allTilesInArea = getAllTilesInArea a b
        allTilesInArea |> List.forall (fun tile -> redOrGreenTiles |> List.contains tile)
    
    printfn "parsing tiles"
    let tiles =
        path
        |> File.ReadAllLines
        |> Array.map parseTile
        |> Array.toList
    
    printfn "finding inside tiles"
    let insideTiles =
        tiles
        |> getBorderTiles
        |> findInsideTiles
    
    printfn "sorting areas"
    let sortedAreas =
        tiles |> getSortedAreas
    
    //print insideTiles
    for l in 0 .. 8 do
        for c in 0 .. 13 do
            if insideTiles |> List.contains { X = int64 c; Y = int64 l } then printf "#" else printf "."
        printfn ""    
            
    sortedAreas
    |> List.find (fun ((a, b), _) -> hasOnlyRedOrGreenTiles insideTiles a b)
    |> fun (_, area) -> area             

let solve () =
    let pathInput = Path.Combine(__SOURCE_DIRECTORY__, "Day9Example.txt")
    printfn $"""part 1 example = %A{solve1 pathInput}"""
    printfn $"""part 2 example = %A{solve2 pathInput}"""
    let pathInput = Path.Combine(__SOURCE_DIRECTORY__, "Day9Input.txt")
    printfn $"""part 1 = %A{solve1 pathInput}"""
    printfn $"""part 2 = %A{solve2 pathInput}"""
    