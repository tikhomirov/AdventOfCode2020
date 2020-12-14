open System.IO
open System.Text.RegularExpressions

let parse pattern command =
    let m = Regex(pattern).Match(command)
    if m.Success
    then Some(List.tail [for g in m.Groups -> g.Value])
    else None

let setBit bit = (|||) (1UL <<< bit)

let clearBit bit = (&&&) (~~~ (1UL <<< bit))

let maskRegex = @"mask = ([X01]{36})"

let memRegex = @"mem\[(\d+)\] = (\d+)"

let (|Mask|_|) =
    let parseMask (mask:string) =
        let parseBit i = function
        | '0' -> Some (clearBit i)
        | '1' -> Some (setBit i)
        | _ -> None

        mask.ToCharArray()
        |> Seq.mapi (fun i c -> (parseBit (35 - i) c))
        |> Seq.collect Option.toList
        |> Seq.reduce (>>)

    parse maskRegex >> Option.map (List.head >> parseMask)

let (|MaskV2|_|) =
    let parseMaskV2 (mask:string) : (uint64 -> uint64) list =
        let bits =
            mask.ToCharArray()
            |> List.ofArray
            |> List.mapi (fun i c -> (35 - i, c))
    
        let rec buildMasks operations = function
            | [] -> operations |> List.reduce (>>) |> List.singleton
            | (i, 'X') :: bits' ->
                List.concat [
                    buildMasks ((clearBit i) :: operations) bits';
                    buildMasks ((setBit i) :: operations) bits']
            | (_, '0') :: bits' -> buildMasks operations bits'
            | (i, '1') :: bits' -> buildMasks ((setBit i) :: operations) bits'
            | _ -> failwith "Invalid mask"
    
        buildMasks [] bits

    parse maskRegex >> Option.map (List.head >> parseMaskV2)

let (|Mem|_|) =
    let parseMem = function
    | [address; value] -> (uint64 address, uint64 value)
    | _ -> failwith "Invalid mem instruction"

    parse memRegex >> Option.map parseMem

let rec eval mask memory = function
    | command :: program' ->
        match command with
        | Mem (address, value) ->
            let memory' = memory |> Map.add address (mask value)
            eval mask memory' program'
        | Mask mask' -> eval mask' memory program'
        | _ -> failwith "Unknown command"
    | [] -> memory

let rec evalV2 mask memory = function
    | command :: program' ->
        match command with
        | Mem (address, value) ->
            let folder m a = (Map.add a value m)
            let memory' = mask |> List.map ((|>) address) |> Seq.fold folder memory
            evalV2 mask memory' program'
        | MaskV2 mask' -> evalV2 mask' memory program'
        | _ -> failwith "Unknown command"
    | [] -> memory

[<EntryPoint>]
let main argv =
    let program = File.ReadLines(argv.[0]) |> List.ofSeq

    program
    |> eval id Map.empty
    |> Map.fold (fun acc _ v -> acc + v) 0UL
    |> printfn "%A"

    program
    |> evalV2 [id] Map.empty
    |> Map.fold (fun acc _ v -> acc + v) 0UL
    |> printfn "%A"

    0