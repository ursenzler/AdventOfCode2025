module Days.Day4

open System.IO

type Cells = { Cells: bool array; Width: int }

let parseFile path =
    let lines = File.ReadAllLines path
    let width = lines |> Array.head |> String.length
    let cells = Array.create ((lines |> Array.length) * width) false
    
    for y, line in lines |> Array.indexed do
        for x, c in line.ToCharArray() |> Seq.indexed do
            cells[y * width + x] <- c = '@'
    {
        Cells = cells
        Width = width
    }
            
let getNumberOfNeighbors (cells: Cells) i =
    let width = cells.Width
    let cells = cells.Cells
    let total = cells.Length
    let tl = if i > width && i % width > 0 then Some cells[i - width - 1] else None
    let t = if i - width >= 0 then Some cells[i - width] else None
    let tr = if i - width + 1 >= 0 && i % width < width - 1 then Some cells[i - width + 1] else None
    let l = if i - 1 >= 0 && i % width > 0 then Some cells[i - 1] else None
    let r = if i % width < width - 1 then Some cells[i + 1] else None
    let bl = if i + width - 1 < total && i % width > 0 then Some cells[i + width - 1] else None
    let b = if i + width < total then Some cells[i + width] else None
    let br = if i + width + 1 < total && i % width < width - 1 then Some cells[i + width + 1] else None    

    [tl; t; tr; l; r; bl; b; br]
    |> List.choose id
    |> List.filter id
    |> List.length

let getMovableRolls (cells: Cells) =
    [|
        for i in 0 .. cells.Cells.Length - 1 do
            if cells.Cells[i] then
                if getNumberOfNeighbors cells i < 4 then yield i
    |]

let solveImpl1 = parseFile >> getMovableRolls >> Array.length

let solveImpl2 path  =
    let cells = parseFile path
    
    cells
    |> Array.unfold (fun cells ->
        let movableRolls = cells |> getMovableRolls
        match movableRolls.Length with
        | 0 -> None
        | m ->
            for i in movableRolls do
                cells.Cells[i] <- false
            Some (m, cells)
        )
    |> Array.sum
    

let solve () =
    let pathInput1 = Path.Combine(__SOURCE_DIRECTORY__, "Day4Input1.txt")
    printfn $"""%A{solveImpl1 pathInput1}"""
    printfn $"""%A{solveImpl2 pathInput1}"""
