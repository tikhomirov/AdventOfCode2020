open System.IO

let readFile = File.ReadLines >> Seq.map int64

let isValid win =
    let x = Array.last win
    let xs = Array.take ((Array.length win) - 1) win
    Array.allPairs xs xs
    |> Array.filter (fun (x1, x2) -> x1 <> x2)
    |> Array.exists (fun (x1, x2) -> (x1 + x2) = x)

let rec findSeq n data =
    let rec take data' xs =
        let xs' = (List.head data') :: xs
        if (List.sum xs') >= n
        then xs'
        else take (List.tail data') xs'

    let xs = take data List.empty

    if List.sum(xs) = n
    then xs
    else findSeq n (List.tail data) 

[<EntryPoint>]
let main argv =
    let data = argv |> Array.head |> readFile

    let preambleLength = 25

    let invalidNumber =
        data
        |> Seq.windowed (preambleLength + 1)
        |> Seq.find (isValid >> not)
        |> Array.last
    printfn "%d" invalidNumber

    let xs = data |> List.ofSeq |> findSeq invalidNumber

    (List.min xs) + (List.max xs)
    |> printfn "%d"
    0
