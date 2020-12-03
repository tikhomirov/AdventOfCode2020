let parseLine (line:string) = line.ToCharArray() |> List.ofArray

let readTreeMap = System.IO.File.ReadLines >> Seq.map parseLine

let traverse rightStep downStep treeMap =
    treeMap
    |> Seq.mapi (fun i line ->
        let right = (i / downStep) * rightStep % List.length line
        if i % downStep = 0 then [Seq.item right line] else [])
    |> Seq.collect id
    |> Seq.filter ((=) '#')
    |> Seq.length

[<EntryPoint>]
let main argv =
    let map = argv |> Array.head |> readTreeMap

    map |> traverse 3 1 |> printfn "%A"

    [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)]
    |> List.map (fun (right, down) -> traverse right down map)
    |> List.map int64
    |> List.fold (*) 1L
    |> printfn "%A"

    0