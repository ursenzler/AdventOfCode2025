[<AutoOpen>]
module Utilities

open System

[<RequireQualifiedAccess>]
module Int =
    let tryParse (value: string) =
        match Int32.TryParse value with
        | true, num -> Some num
        | _ -> None
        
[<RequireQualifiedAccess>]        
module Int64 =        
    let tryParse (value: string) =
        match Int64.TryParse value with
        | true, num -> Some num
        | _ -> None        

