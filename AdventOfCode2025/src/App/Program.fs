// For more information see https://aka.ms/fsharp-console-apps
open Days
open System

// Dispatch table: map numbers to functions
let dispatch n =
    match n with
    | 1 -> Day1.solve 
    | _ ->
        // default: print available choices and exit
        failwithf "Unknown function id: %d." n

[<EntryPoint>]
let main (argv: string[]) =
    if argv.Length = 0 then failwith "provide day"
    else
        match Int32.TryParse(argv.[0]) with
        | (true, n) -> dispatch n
        | _ -> failwith "provide day"
        0