[<EntryPoint>]
let main argv =
    let report =
        argv
        |> Array.tryHead
        |> Option.defaultValue "input1.txt"
        |> System.IO.File.ReadLines
        |> Seq.map int

    let reportSet = Set.ofSeq report
    let existsInReport n = Set.contains n reportSet

    report
    |> Seq.find (fun n -> 2020 - n |> existsInReport)
    |> fun n -> n * (2020 - n)
    |> printfn "%d"

    report
    |> Seq.allPairs report
    |> Seq.find (fun (n1, n2) -> 2020 - n1 - n2 |> existsInReport)
    |> fun (n1, n2) -> n1 * n2 * (2020 - n1 - n2)
    |> printfn "%d"

    0