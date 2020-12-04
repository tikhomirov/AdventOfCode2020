let asPair = function
    | [field; value] -> (field, value)
    | list -> failwithf "Incorrect data %A" list

let split (c:char) (str:string) = str.Split(c) |> List.ofSeq

let parseField = split ':' >> asPair

let parseLine = split ' ' >> List.map parseField

let foldLine passports = function
    | "" ->
        List.empty :: passports
    | line ->
        let passport =
            passports
            |> List.head
            |> List.append (parseLine line)
        passport :: (List.tail passports)

let readPassports(path) =
    System.IO.File.ReadLines(path)
    |> Seq.fold foldLine (List.singleton [])
    |> Seq.map Map.ofList

let (|Parse|_|) regex str =
    let m = System.Text.RegularExpressions.Regex(regex).Match(str)
    if m.Success then Some (List.tail [for g in m.Groups -> g.Value])
    else None

let (|HeightCM|_|) str =
    match str with
    | Parse @"^(\d{3})cm$" [cm] -> Some (HeightCM (int cm))
    | _ -> None

let (|HeightIN|_|) = function
    | Parse @"^(\d{2})in$" [inch] -> Some (HeightIN (int inch))
    | _ -> None

let requiredKeys = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]

let hasAllRequiredFields passport =
    requiredKeys
    |> List.forall (fun key -> Map.containsKey key passport)

let hasValidBirthYear passport =
    let byr = passport |> Map.find "byr" |> int
    (byr >= 1920) && (byr <= 2002)

let hasValidIssueYear passport =
    let iyr = passport |> Map.find "iyr" |> int
    (iyr >= 2010) && (iyr <= 2020)

let hasValidExpirationYear passport =
    let eyr = passport |> Map.find "eyr" |> int
    (eyr >= 2020) && (eyr <= 2030)

let hasValidHeight passport =
    let hgt = passport |> Map.find "hgt"
    match hgt with
    | HeightCM cm -> (cm >= 150) && (cm <= 193)
    | HeightIN inch -> (inch >= 59) && (inch <= 76)
    | _ -> false

let hasValidHairColor passport =
    let hcl = passport |> Map.find "hcl"
    match hcl with
    | Parse @"^#[0-9a-f]{6}$" _ -> true
    | _ -> false

let eyeColors = ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] |> Set.ofList

let hasValidEyeColor passport =
    let ecl = passport |> Map.find "ecl"
    eyeColors |> Set.contains ecl

let hasValidPassportId passport =
    let pid = passport |> Map.find "pid"
    match pid with
    | Parse @"^[0-9]{9}$" _ -> true
    | _ -> false

let validators = [
    hasValidBirthYear;
    hasValidIssueYear;
    hasValidExpirationYear;
    hasValidHeight;
    hasValidHairColor;
    hasValidEyeColor;
    hasValidPassportId]

let hasAllValidFields passport = List.forall (fun validator -> validator passport) validators

[<EntryPoint>]
let main argv =
    let passports = argv |> Array.head |> readPassports

    passports
    |> Seq.filter hasAllRequiredFields
    |> Seq.length
    |> printfn "%A"

    passports
    |> Seq.filter hasAllRequiredFields
    |> Seq.filter hasAllValidFields
    |> Seq.length
    |> printfn "%A"

    0
