open System.IO

let chars (str:string) = str.ToCharArray() |> List.ofArray

let ones = Set.ofList ['B'; 'R']
let bit c = if ones |> Set.contains c then 1 else 0

let seatNumber line =
    let rec parse' = function
    | c :: chars' ->
        let b = (bit c) <<< List.length chars'
        b ^^^ parse' chars'
    | [] -> 0

    line |> (chars >> parse')

let readSeats path =
    File.ReadLines(path)
    |> Seq.map seatNumber
    |> Set.ofSeq

[<EntryPoint>]
let main argv =
    let seats = argv |> Array.head |> readSeats

    seats
    |> Set.maxElement
    |> printfn "%A"

    let exists seat = Set.contains seat seats
    let minSeat = Set.minElement seats
    let maxSeat = Set.maxElement seats

    seq { (minSeat + 1) .. (maxSeat - 1) }
    |> Seq.find (not << exists)
    |> printfn "%A"

    0