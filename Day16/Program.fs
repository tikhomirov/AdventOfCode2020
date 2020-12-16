let parseTicket (line:string) = line.Split(",") |> Seq.map int64 |> List.ofSeq

let parseRule (line:string) =
    let splitPoint = line.IndexOf(": ")
    let ruleName = line.Substring(0, splitPoint)
    let validations =
        line.Substring(splitPoint + 2).Split(" or ")
        |> Seq.map (fun value -> value.Split("-") |> Seq.map int64)
        |> Seq.map (fun range -> Seq.head range, Seq.last range)

    (ruleName, validations)

let rec isValid rules = function
    | [] ->
        rules |> Map.isEmpty
    | field :: restFields ->
        rules
        |> Map.tryFindKey (fun _ ranges -> ranges |> Seq.exists (fun (min, max) -> field >= min && field <= max))
        |> Option.map (fun rule -> isValid (Map.remove rule rules) restFields)
        |> Option.defaultValue false

let rangeContains field (min, max) = field >= min && field <= max

let fieldsByRule rules ticket =
    let fields = ticket |> Seq.mapi (fun i field -> (i, field))
    
    let validFields ranges =
        fields
        |> Seq.filter (fun (_, field) -> ranges |> Seq.exists (rangeContains field))

    rules |> Map.map (fun _ ranges -> validFields ranges |> Seq.map fst)

let rec findInvalidFields rules = function
    | [] -> []
    | field :: tail ->
        let tailErrors = findInvalidFields rules tail
        rules
        |> Map.tryFindKey (fun _ ranges -> ranges |> Seq.exists (rangeContains field))
        |> Option.map (fun _ -> tailErrors)
        |> Option.defaultValue (field :: tailErrors)

[<EntryPoint>]
let main argv =
    let lines = System.IO.File.ReadLines(argv |> Array.head)
    
    let rules =
        lines
        |> Seq.takeWhile ((<>) "")
        |> Seq.map parseRule
        |> Map.ofSeq

    let myTicket =
        lines
        |> Seq.skipWhile ((<>) "your ticket:")
        |> Seq.skip 1   
        |> Seq.head
        |> parseTicket

    let tickets =
        lines
        |> Seq.skipWhile ((<>) "nearby tickets:")
        |> Seq.skip 1
        |> Seq.map parseTicket
        |> List.ofSeq

    tickets
    |> List.collect (findInvalidFields rules)
    |> List.sum
    |> printfn "%A"

    let isValidValue value ranges =
        ranges |> Seq.exists (fun (min, max) -> (value >= min) && (value <= max))

    let columns = Seq.initInfinite id |> Seq.take myTicket.Length 

    let validTickets = tickets |> List.ofSeq |> List.filter (isValid rules)

    let possibleColumns ranges =
        columns
        |> Seq.where (fun i -> validTickets |> Seq.forall (fun ticket -> isValidValue ticket.[i] ranges))
        |> List.ofSeq

    let rec findFields (fieldColumns: Map<string, list<int>>) fields =
        if fieldColumns |> Map.isEmpty
        then
            fields
        else
            let field = fieldColumns |> Map.findKey (fun _ columns -> (List.length columns) = 1)
            let column = fieldColumns |> Map.find field |> Seq.head
            let fieldColumns' =
                fieldColumns
                |> Map.remove field
                |> Map.map (fun _ columns -> List.except (Seq.singleton column) columns)

            findFields fieldColumns' ((field, column) :: fields)

    let fields = findFields (rules |> Map.map (fun _ ranges -> possibleColumns ranges)) List.empty |> Map.ofSeq

    fields
    |> Map.filter (fun name _ -> name.StartsWith("departure"))
    |> Map.map (fun _ column -> myTicket.[column])
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.reduce (*)
    |> printfn "%A"

    0