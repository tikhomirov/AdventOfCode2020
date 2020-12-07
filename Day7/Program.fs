open System.IO
open System.Text

let regex pattern str = RegularExpressions.Regex(pattern).Match(str)

let parseRule (str:string) =
    let m = regex @"(\d+) (\w+ \w+)" (str.Trim())
    if m.Success
    then
        let num = m.Groups.[1].Value |> int
        let bag = m.Groups.[2].Value
        (num, bag)
    else failwith "incorrect input"

let parseLine (line:string) =
    match line.Split(" bags contain ") |> List.ofArray with
    | [bag; rules] ->
        let parsedRules =
            if rules = "no other bags."
            then List.empty
            else
                rules.Split(',')
                |> List.ofArray
                |> List.map parseRule
        bag, parsedRules
    | _ -> failwith "invalid input"

let readRules path = File.ReadLines(path) |> Seq.map parseLine |> Map.ofSeq

[<EntryPoint>]
let main argv =
    let rules = argv |> Array.head |> readRules

    let rec contains color name =
        rules
        |> Map.find name
        |> List.exists (fun (_, name') -> (name' = color) || contains color name')

    rules
    |> Map.toList
    |> List.map fst
    |> List.filter (contains "shiny gold")
    |> List.length
    |> printfn "%A"
    
    let rec count color =
        Map.find color rules
        |> List.map (fun (c, color') -> c * ((count color') + 1))
        |> List.sum
    count "shiny gold" |> printfn "%A"

    0