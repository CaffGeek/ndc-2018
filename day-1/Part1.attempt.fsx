// 1. GETTING SOME DATA
open System
open System.IO
open System.Diagnostics

type LabelPixel = { Label:int; Pixels:int[] }
type Distance = { Label: int; Distance: float }
type Result = { Actual: int; Predicted: int }

let fileAsRecords (fileName: string) =
    let localPath = __SOURCE_DIRECTORY__
    let dataPath = Path.Combine(localPath,"digits")
    let fileName = Path.Combine(dataPath, fileName)
    let data = File.ReadAllLines(fileName)
    // 2. EXTRACTING COLUMNS
    let lines = data |> Array.map(fun s -> s.Split(','))
    // 3. CLEANING UP HEADERS
    let dataLines = lines.[1..]
    // 4. CONVERTING FROM STRINGS TO INTS
    let ints = dataLines |> Array.map(fun x -> x |> Array.map(fun s -> (int)s))
    // 5. CONVERTING ARRAYS TO RECORDS
    ints |> Array.map(fun x -> { Label = x.[0]; Pixels = x.[1..]})

// 6. COMPUTING DISTANCES
let squareDistance (P1: int[]) (P2: int[]) =
    (P1, P2)
    ||> Array.map2 (fun p1 p2 -> (p1-p2)*(p1-p2)) 
    |> Array.sum
    |> float
    |> Math.Sqrt

let absDistance (P1: int[]) (P2: int[]) =
    (P1, P2)
    ||> Array.map2 (fun p1 p2 -> Math.Abs((int)p1-p2))
    |> Array.sum
    |> float

// 7. WRITING THE CLASSIFIER FUNCTION
let mode (l:int[]) =
    let x = 
        l
        |> Array.countBy (fun item -> item)             // Count individual items
        |> Array.fold                                   // Find max counts
            (fun (cp, lst) (item, c) ->                 // State is (count, list of items with that count)
                if c > cp then (c, [item])              // New max - keep count and a list of the single item
                elif c = cp then (c, item :: lst)       // New element with max count - prepend it to the list
                else (cp,lst))                          // else just keep old count/list
            (0, [])                                     // Start with a count of 0 and a dummy item
        |> snd                                          // From (count, list) we just want the second item (the list)
    x.[0]

let knnclassify (distanceFn: int[] -> int[] -> float) (k:int) (trainedRecords: LabelPixel[]) (unknown:int[]) =
    let sorted = 
        trainedRecords 
        |> Array.map(fun r -> { Label = r.Label; Distance = distanceFn r.Pixels unknown })
        |> Array.sortBy(fun x-> x.Distance)
    let top = 
        sorted.[0..k-1]
        |> Array.map(fun x -> x.Label)
    mode top

// 8. EVALUATING THE MODEL AGAINST VALIDATION DATA
let execute (filename: string) (classifier: int[] -> int) =
    fileAsRecords filename
    |> Array.averageBy(fun x -> if x.Label = classifier (x.Pixels) then 1.0 else 0.0)

let executeWitValidationSample = 
    execute "validationsample.csv" 

let resultsquarek1 = 
    fileAsRecords "trainingsample.csv"
    |> knnclassify squareDistance 1
    |> executeWitValidationSample 
let resultsquarek3 = 
    fileAsRecords "trainingsample.csv"
    |> knnclassify squareDistance 3
    |> executeWitValidationSample 
    
let resultabsk1 = 
    fileAsRecords "trainingsample.csv"
    |> knnclassify absDistance 1
    |> executeWitValidationSample 
let resultabsk3 = 
    fileAsRecords "trainingsample.csv"
    |> knnclassify absDistance 3
    |> executeWitValidationSample 