module Days.Day6

open System
open System.IO
open FSharpx

let parseLine line =
    line
    |> String.splitCharWithOptions [| ' ' |] StringSplitOptions.RemoveEmptyEntries  

let calculate (exercise: string[]) =
    let values = exercise[0..exercise.Length-2] |> Array.map int64
    let operator = exercise |> Array.last
    
    match operator with
    | "+" -> values |> Array.sum
    | "*" -> values |> Array.reduce (*)
    | _ -> failwith $"Invalid operator {operator}"    

let solve1 path =
    path
    |> File.ReadAllLines
    |> Array.map parseLine
    |> Array.transpose
    |> Array.map calculate
    |> Array.sum

let findColumnEndIndex (lines: string[])=
    lines
    |> Array.map (fun line ->
        line
        |> String.indexOfChar ' '
        |> function
            | -1 -> line.Length
            | x -> x)
    |> Array.max
   
let isEmptyColumn (column: string[]) =
    column |> Array.forall (fun x -> x = "")
    
let getColumn (lines: string[]) =
    let columnEndIndex = lines |> findColumnEndIndex
    let x =
        lines
        |> Array.map (fun line ->
            line[0..columnEndIndex-1], line[columnEndIndex+1..])
    let column = x |> Array.map fst
    let rest = x |> Array.map snd |> fun x -> if isEmptyColumn x then None else Some x 
    column, rest
    
let getColumns (lines: string[]) =
    lines
    |> Some
    |> Array.unfold (fun rest ->
        match rest with
        | None -> None
        | Some rest -> getColumn rest |> Some
    )
 
let extractNumbersAndOperatorFromColumn (column: string[]) =
    let width = column[0].Length
    let numbers = column[0..column.Length-2]
    let operator = column.[column.Length-1] |> String.trim
    [|
        for i in width-1..-1..0 do
            numbers |> Array.map (fun x -> $"{x[i]}") |> String.concat ""
        operator    
    |]

let solve2 path =
    path
    |> File.ReadAllLines
    |> getColumns
    |> Array.map extractNumbersAndOperatorFromColumn
    |> Array.map calculate
    |> Array.sum

let solve () =
    let pathInput = Path.Combine(__SOURCE_DIRECTORY__, "Day6Input.txt")
    printfn $"""%A{solve1 pathInput}"""
    printfn $"""%A{solve2 pathInput}"""

