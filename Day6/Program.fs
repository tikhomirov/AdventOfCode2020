open System.IO

let answers (line:string) = line.ToCharArray() |> Set.ofArray

let readGroups path =
    let folder groups =
        function
        | "" -> [] :: groups
        | line ->
            let group' = (answers line) :: (List.head groups)
            group' :: (List.tail groups)

    File.ReadLines(path) |> Seq.fold folder [[]]

[<EntryPoint>]
let main argv =
    let groups = argv |> Array.head |> readGroups

    groups
    |> List.map (Set.unionMany >> Set.count)
    |> List.sum
    |> printfn "%A"

    groups
    |> List.map (Set.intersectMany >> Set.count)
    |> List.sum
    |> printfn "%A"

    0