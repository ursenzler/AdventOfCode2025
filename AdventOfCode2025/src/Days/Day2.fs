module Days.Day2

open System.IO
open FSharpx

// this is an active pattern that can be used in pattern matching
let (|Int|_|) text = Int64.tryParse text 
    
// is it a valid ID? (part 1)    
let (|Invalid|_|) text =
    let l = text |> String.length
    if l % 2 = 0 then
        let half = l / 2
        if text[0..half-1] = text[half..] then Some text else None
    else None
    
// the "well-known" regex active pattern    
let (|Regex|_|) regex str =
   let m = System.Text.RegularExpressions.Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None    
    
// is it a repeating ID? (part 2)    
let (|Repeating|_|) text =
    let l = text |> String.length
    [1..(l / 2)]
    |> List.map (fun l -> text[0..l-1])
    |> List.exists (fun candidate ->
        let pattern = $@"^({candidate})+$"
        match text with
        | Regex pattern [ _ ] -> true
        | _ -> false
    )
    
// a tuple would be good enough, but showing types and decomposition further down    
type Data = { FromInt: int64; ToInt: int64 }

let rangeFromStrings strFrom strTo =
    match Int64.tryParse strFrom, Int.tryParse strTo with
    | Some fromId, Some toId -> Some(fromId, toId)
    | _ -> failwith "Could not ids"

let readRanges text =
    text
    |> String.splitChar [| ',' |] // FSharpX provides F#-ish string functions
    |> Array.map (String.splitChar [| '-' |])
    |> Array.choose (function
        // ignore everything that is not a int - int pair
        // now we use the active pattern
        | [| Int fromInt; Int toInt |] -> Some { FromInt = fromInt; ToInt = toInt }
        | _ -> None)
    
let solve' path (findInvalidRanges: Data -> int64 array) =
    path
    |> File.ReadAllText
    |> readRanges
    |> Array.collect findInvalidRanges // collect the numbers into a single array before summing
    |> Array.sum
    
let findInvalidRanges1 { FromInt = fromInt; ToInt = toInt } =
    // array comprehension for some imperative programming because we can in F#
    [|
        for i in [ fromInt .. toInt ] do
            match i.ToString() with
            | Invalid _ -> i
            | _ -> ()
    |]
    
let findInvalidRanges2 { FromInt = fromInt; ToInt = toInt } =
    [|
        for i in [ fromInt .. toInt ] do
            match i.ToString() with
            | Repeating -> i
            | _ -> ()
    |]    

let solve () =
    let pathInput1 = Path.Combine(__SOURCE_DIRECTORY__, "Day2Input1.txt")
    printfn $"""%A{solve' pathInput1 findInvalidRanges1}"""
    printfn $"""%A{solve' pathInput1 findInvalidRanges2}"""

