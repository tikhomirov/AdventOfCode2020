open System.IO

let (|ACC|JMP|NOP|) (line:string) =
    let [|op; arg|] = line.Split(' ')
    match op with
    | "acc" -> ACC (int arg)
    | "jmp" -> JMP (int arg)
    | _ -> NOP (int arg)

let evalLoop (lines:string []) =
    let rec eval' pc acc visited =
        if Set.contains pc visited then
            acc
        else
            let visited' = visited |> Set.add pc
            match lines.[pc] with
            | ACC n -> eval' (pc + 1) (acc + n) visited'
            | JMP n -> eval' (pc + n) acc visited'
            | NOP _ -> eval' (pc + 1) acc visited'

    eval' 0 0 Set.empty

let eval (lines:string []) =
    let rec eval' pc acc visited =
        if pc >= lines.Length then
            Some acc
        else
            if Set.contains pc visited then
                None
            else
                let visited' = visited |> Set.add pc
                match lines.[pc] with
                | ACC n -> eval' (pc + 1) (acc + n) visited'
                | JMP n -> eval' (pc + n) acc visited'
                | NOP _ -> eval' (pc + 1) acc visited'

    eval' 0 0 Set.empty

[<EntryPoint>]
let main argv =
    let code = argv |> Array.head |> File.ReadAllLines

    evalLoop code |> printfn "%A"

    // TODO rewrite this ugly loop
    for i in 0 .. (code.Length - 1) do
        let line' = code.[i]
        let [|op; arg|] = line'.Split(' ')

        let op' = if op = "jmp" then "nop" else "jmp"
        
        code.[i] <- String.concat " " [op'; arg]
        let result = eval code
        if result.IsSome then
            printfn "%A" result.Value
            exit(0)
        else
            code.[i] <- line'

    0