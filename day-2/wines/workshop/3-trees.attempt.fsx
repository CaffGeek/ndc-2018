#load "2-model.fsx"
open 3-trees
open ``2-model``

#I "../packages/Newtonsoft.Json/lib/net45/"
#I "../packages/Google.DataTable.Net.Wrapper/lib/"
#r "../packages/XPlot.GoogleCharts/lib/net45/XPlot.GoogleCharts.dll"

open XPlot.GoogleCharts

let options = Configuration.Options()
options.dataOpacity <- 0.20
options.pointSize <- 10

type Tree = 
    | Prediction of float
    | Branch of (Feature * float) * Tree * Tree


let manualTree = 
    Branch(
        // the feature and level,
        (alcohol,10.0),
        // the low branch,
        Prediction(4.0),
        // and the high branch.
        Prediction(6.0)
        )

let deeperTree =
    Branch(
        // the feature and level,
        (alcohol,10.0),
        // the low branch has a prediction,
        Prediction(4.0),
        // and the high branch is another branch.
        Branch(
            // which splits on Chlorides / 0.85...
            (chlorides,0.85),
            Prediction(5.0),
            Prediction(6.5)
            )
        )

let rec treePredict (tree:Tree) (wine:Wine) =
    match tree with
    | Prediction(value) -> value
    | Branch((feature,level),lowBranch,highBranch) ->
        if (feature wine < level)
        then treePredict lowBranch wine
        else treePredict highBranch wine

treePredict deeperTree (redWines |> Seq.head)
let treePredictor = treePredict deeperTree

let manualCost = treePredict manualTree |> cost sample 
let deeperCost = treePredict deeperTree |> cost sample 

let splitCost (sample:Sample) (feature:Feature) (level:float) =
    let stump = learnStump sample feature level
    cost sample stump

let costOfSplits (sample:Sample) (feature:Feature) (gridSize:int) =
    levels sample feature gridSize
    |> List.map (fun level -> 
        (feature,level), splitCost sample feature level)

type Config = {
    GridSize:int
    MaxDepth:int
    MinLeafSize:int
}

let rec learnTree 
            (config:Config) 
                (features:Feature list) 
                    (sample:Sample) 
                        (depth:int) =
    let sampleSize = sample |> Seq.length
    if (depth >= config.MaxDepth || sampleSize <= config.MinLeafSize) 
    then 
        let prediction = 
            sample 
            |> Seq.averageBy (fun (wine,value) -> value)
        Prediction(prediction)
    else
        let candidates = 
            features
            |> List.collect (fun feature -> 
                costOfSplits sample feature config.GridSize)
        match candidates with
        | [] -> 
            sample 
            |> Seq.averageBy (fun (wine,value) -> value) 
            |> Prediction
        | _ ->
            let (feature,level) = candidates |> Seq.minBy snd |> fst
            let under = 
                sample 
                |> Seq.filter (fun (wine,value) -> feature wine <= level)
            let over = 
                sample 
                |> Seq.filter (fun (wine,value) -> feature wine > level)
            let underTree = learnTree config features under (depth + 1)
            let overTree = learnTree config features over (depth + 1)
            Branch((feature,level),underTree,overTree)
    
// wrap the function to simplify calls
let learn config features sample =
    let tree = learnTree config features sample 0
    treePredict tree

let defaultConfig = {
    GridSize = 10
    MaxDepth = 5
    MinLeafSize = 20
}

let examplePredictor = learn defaultConfig features sample

sample
|> Seq.take 10
|> Seq.map (fun (wine,quality) -> 
    quality, examplePredictor wine)
|> Seq.iter (fun (actual, predicted) ->
    printfn "Actual: %.2f / Predicted: %.2f" actual predicted)

let cost_depth_2 = 
    learn { defaultConfig with MaxDepth = 2 } features sample
    |> cost sample

let cost_depth_6 = 
    learn { defaultConfig with MaxDepth = 6 } features sample
    |> cost sample


let predictor_depth_6 = 
    learn { defaultConfig with MaxDepth = 6 } features sample

let depth_6_quality = 
    sample
    |> Seq.map (fun (wine,quality) ->
        (quality,predictor_depth_6 wine))
    |> Chart.Scatter
    |> Chart.WithOptions options
    |> Chart.Show

let validationConfig = {
    GridSize = 5 // rough grid
    MaxDepth = 1
    MinLeafSize = 20
}

// [ 1 .. 10 ]
// |> List.map (fun depth ->
//     let config = { validationConfig with MaxDepth = depth }
//     let predictor = learn config features sample
//     cost sample predictor
//     )
// |> Chart.Line
// |> Chart.WithXTitle "Depth"
// |> Chart.WithYTitle "Cost"
// |> Chart.Show

let sampleSize = sample |> Seq.length
let trainingSample = sample |> Seq.take (sampleSize/2)
let testingSample = sample |> Seq.skip (sampleSize/2)

[ 1 .. 10 ]
|> List.map (fun depth ->
    let config = { validationConfig with MaxDepth = depth }
    let predictor = learn config features trainingSample  
    let trainingCost = cost trainingSample predictor
    let testingCost = cost testingSample predictor
    (depth,trainingCost),(depth,testingCost))
|> List.unzip
|> fun (train,test) -> [ train; test ]
|> Chart.Line
|> Chart.WithLabels [ "Training"; "Testing" ]
|> Chart.Show

