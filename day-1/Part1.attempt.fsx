// 1. GETTING SOME DATA
open System
open System.IO
open System.Linq
let localPath = __SOURCE_DIRECTORY__
let dataPath = Path.Combine(localPath,"digits")
let fileName = Path.Combine(dataPath, "trainingsample.csv")
let data = File.ReadAllLines(fileName)

// 2. EXTRACTING COLUMNS
let lines = data |> Array.map(fun s -> s.Split(','))

// 3. CLEANING UP HEADERS
let headers = lines.[0]
let dataLines = lines.[1..]

// 4. CONVERTING FROM STRINGS TO INTS
let ints = dataLines |> Array.map(fun x -> x |> Array.map(fun s -> (int)s))

// 5. CONVERTING ARRAYS TO RECORDS
type LabelPixel = { Label:int; Pixels:int[] }
let records = ints |> Array.map(fun x -> { Label = x.[0]; Pixels = x.[1..]})

// 6. COMPUTING DISTANCES
let distance (P1: int[]) (P2: int[]) =
    Array.map2 (fun p1 p2 -> (p1-p2)*(p1-p2)) P1 P2
    |> Array.sum
    |> float
    |> Math.Sqrt

// 7. WRITING THE CLASSIFIER FUNCTION
type Distance = { Label: int; Distance: float }
let classify (unknown:int[]) =
    let prediction = 
        records 
        |> Array.map(fun r -> { Label = r.Label; Distance = distance r.Pixels unknown })
        |> Array.minBy(fun x -> x.Distance)
    prediction.Label

// 8. EVALUATING THE MODEL AGAINST VALIDATION DATA

let validationFileName = Path.Combine(dataPath, "validationsample.csv")
let validationData = File.ReadAllLines(validationFileName)

type Result = { Actual: int; Predicted: int}
let validationRecords = 
    validationData.[1..] 
    |> Array.map(fun s -> s.Split(','))
    |> Array.map(fun x -> x |> Array.map(fun s -> (int)s))
    |> Array.map(fun x -> { Label = x.[0]; Pixels = x.[1..] })
    |> Array.map(fun x -> { Actual = x.Label; Predicted = classify x.Pixels })

let passes = 
    validationRecords
    |> Array.filter(fun x -> x.Actual = x.Predicted)
    
let percentage =
    (float)passes.Length / (float)validationRecords.Length