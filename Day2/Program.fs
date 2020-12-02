open System.IO
open System.Text.RegularExpressions

let (|Entry|) input =
    let m = Regex(@"(\d+)-(\d+) (\w): (\w+)").Match(input)
    List.tail <| [for g in m.Groups -> g.Value]

let parse line =
    match line with
    | Entry [i; j; c; password] ->
        int(i), int(j), c.Chars(0), List.ofArray <| password.ToCharArray()
    | _ -> failwithf "Can't parse line: %s" line

let isValidCount (min, max, c, password) =
    password
    |> List.filter ((=) c)
    |> List.length
    |> fun count -> (seq { min .. max }) |> Seq.contains count 

let isValidPosition (first, second, c, password) =
    let first' = List.item (first - 1) password
    let second' = List.item (second - 1) password
    ((first' = c) || (second' = c)) && (first' <> second')

let readDatabase = File.ReadLines >> Seq.map parse

[<EntryPoint>]
let main argv =
    let database = argv |> Array.head |> readDatabase

    database
    |> Seq.filter isValidCount
    |> Seq.length
    |> printfn "%d"

    database
    |> Seq.filter isValidPosition
    |> Seq.length
    |> printfn "%d"

    0