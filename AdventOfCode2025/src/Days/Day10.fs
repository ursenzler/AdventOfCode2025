module Days.Day10

open System
open System.IO
open FSharpx

type IndicatorLights = array<bool>
type Button = list<int>
type Buttons = list<Button>
type Joltages = array<int>

let parseLine line =
    let parts = line |> String.splitChar [| ' ' |] |> Array.toList

    let indicatorLights: IndicatorLights =
        parts[0]
        |> String.toCharArray
        |> Array.choose (function
            | '#' -> Some true
            | '.' -> Some false
            | _ -> None)

    let buttons: Buttons =
        parts
        |> List.tail
        |> List.choose (function
            | str when String.startsWith "(" str ->
                str
                |> String.splitChar [| ','; '('; ')' |]
                |> Array.toList
                |> List.choose Int.tryParse
                |> Some
            | _ -> None)

    let joltage: Joltages =
        parts
        |> List.last
        |> (function
        | str when String.startsWith "{" str ->
            str
            |> String.splitChar [| ','; '{'; '}' |]
            |> Array.choose Int.tryParse
            |> Some
        | _ -> None)
        |> Option.getOrFail "could not process joltage"

    indicatorLights, buttons, joltage

let parseFile = File.ReadAllLines >> Array.map parseLine >> Array.toList

let solve1 path =
    
    let applyButton (indicatorLights: IndicatorLights) (button: Button) =
        indicatorLights
        |> Array.mapi (fun i v -> if button |> List.contains i then not v else v)
    
    let rec minimalPresses indicatorLights (buttons: Buttons) =
        // getting from initially all-false to the requested configuration requires the
        // same presses as vice versa and sequence of presses is irrelevant
        // pressing buttons twice undoes its action, so we may only consider pressing a button or not
        let allOff = Array.forall not indicatorLights

        match buttons, allOff with
        | _, true -> Some 0
        | [], false -> None
        | head :: tail, false ->
            let withoutPressingTheButton = minimalPresses indicatorLights tail
            let withPressingTheButton = minimalPresses (applyButton indicatorLights head) tail
            match withoutPressingTheButton, withPressingTheButton with
            | Some x, Some y -> min x (1 + y) |> Some
            | None, Some y -> (1 + y) |> Some
            | Some x, None -> Some x
            | None, None -> None
    
    path
    |> parseFile
    |> List.map (fun (indicatorLights, buttons, _) -> minimalPresses indicatorLights buttons)
    |> List.choose id
    |> List.sum
    

type X = BelowZero | Zero |AboveZero    
let solve2 path =
    
    let applyButton (joltages: Joltages) (button: Button) times =
        joltages
        |> Array.mapi (fun i v -> if button |> List.contains i then v - times else v)
        
    
    let enumerateButtonPresses (joltages: Joltages) (buttons: Buttons) =
        let maxJoltage = joltages |> Array.max
        let buttonsArray = Array.ofList buttons
        let nButtons = buttonsArray.Length
        let queue = System.Collections.Generic.Queue()

        // State: (current joltages, counts of presses per button, last button index processed)
        // We track lastIdx to avoid duplicates (permutations): only increment indices >= lastIdx
        queue.Enqueue((Array.zeroCreate nButtons, 0, 0))

        seq {
            while queue.Count > 0 do
                let currCounts, lastIdx, totalPresses = queue.Dequeue()

                if totalPresses >= maxJoltage then
                    yield currCounts

                for i in lastIdx .. nButtons - 1 do
                    let nextCounts = Array.copy currCounts
                    nextCounts[i] <- nextCounts[i] + 1

                    queue.Enqueue((nextCounts, i, totalPresses + 1))
        }
        
    let checkPresses (joltages: Joltages) (buttons: Buttons) (presses: int array) =
        //printfn $"""checking {presses |> Array.map string |> String.concat ","} on {joltages |> Array.map string |> String.concat ","} with {buttons}"""
        presses
        |> Array.mapi tuple2
        |> Array.fold
            (fun acc (i, presses) ->
                let nn = applyButton acc buttons[i] presses
                nn
            )
            joltages
        |> Array.forall ((=) 0)            
    
    path
    |> parseFile
    |> List.head
    |> fun (_, buttons, joltages) -> enumerateButtonPresses joltages buttons
    |> Seq.take 20
        
        
    // path
    // |> parseFile
    // |> List.map (fun (_, buttons, joltage) ->
    //     enumerateButtonPresses joltage buttons
    //     |> Seq.find (checkPresses joltage buttons))
    // |> List.map Array.sum
    // |> List.sum

let solve () =
    let pathInput = Path.Combine(__SOURCE_DIRECTORY__, "Day10Example.txt")
    //printfn $"""part 1 example = %A{solve1 pathInput}"""
    printfn $"""part 2 example = %A{solve2 pathInput}"""
    let pathInput = Path.Combine(__SOURCE_DIRECTORY__, "Day10Input.txt")
    //printfn $"""part 1 = %A{solve1 pathInput}"""
    printfn $"""part 2 = %A{solve2 pathInput}"""
    