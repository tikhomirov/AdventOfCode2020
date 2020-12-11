open System.IO

let readData = File.ReadLines >> Seq.map int >> Seq.sort

[<EntryPoint>]
let main argv =
    let chargers = argv |> Array.head |> readData
    let device = chargers |> Seq.last |> ((+) 3)
    let joltages = seq { yield 0; yield! chargers; yield device }

    joltages |> Seq.iter (printf "%A ")

    let differences =
        Seq.pairwise joltages
        |> Seq.map (fun (i, j) -> j - i)
        |> Seq.countBy id
        |> Map.ofSeq

    (Map.find 1 differences) * (Map.find 3 differences)
    |> printfn "%A"

    0