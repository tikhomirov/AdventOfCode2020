//let inputString = "0,3,6"
//let inputString = "1,3,2"
let inputString = "1,2,16,19,18,0"

let input = inputString.Split(",") |>  Seq.map int

let play input lastStep =
    let rec play' current memo = function
    | step when step = lastStep ->
        current
    | step ->
        let next =
            memo
            |> Map.tryFind current
            |> Option.map ((-) step)
            |> Option.defaultValue 0

        let memo' = Map.add current step memo
        play' next memo' (step + 1)

    let memo = input |> Seq.mapi (fun i x -> x, i + 1) |> Map.ofSeq
    play' 0 memo (Seq.length input + 1)

[<EntryPoint>]
let main _ =

    play input 2020 |> printfn "%A"
    play input 30000000 |> printfn "%A"

    0