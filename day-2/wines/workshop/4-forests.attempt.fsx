#load "2-model.fsx"
open ``2-model``

#I "../packages/Newtonsoft.Json/lib/net45/"
#I "../packages/Google.DataTable.Net.Wrapper/lib/"
#r "../packages/XPlot.GoogleCharts/lib/net45/XPlot.GoogleCharts.dll"

open XPlot.GoogleCharts

let options = Configuration.Options()
options.dataOpacity <- 0.20
options.pointSize <- 10

#r @"../packages/alglibnet2/lib/alglibnet2.dll"

type Config = {
    Trees:int // how many trees to train
    FeaturesIncluded:float // % features to keep
    LearningProportion:float // % examples to keep
}

let prepareData (features:Feature list) (sample:Example seq) =
    sample
    |> Seq.map (fun (wine,quality) ->
        [|
            for feature in features ->
                feature wine
            yield quality
        |])
    |> array2D

let learn (config:Config) (features:Feature list) (sample:Sample) =
    let trainingset = prepareData features sample        
    let nFeatures = features.Length    
    let _,forest,report = 
        alglib.dfbuildrandomdecisionforestx1(
            trainingset,
            trainingset.GetUpperBound(0),
            nFeatures,
            1, // regression
            config.Trees,
            (float nFeatures) * config.FeaturesIncluded |> ceil |> int,
            config.LearningProportion
            )
    let predict (input:float[]) =
        let mutable result = Array.empty<float>
        alglib.dfprocess(forest, input, &result)
        result.[0]
    let predictor (wine:Wine) =
        features 
        |> List.map (fun feature -> feature wine)
        |> List.toArray
        |> predict
    report, predictor

let forestConfig = {
    Trees = 100
    FeaturesIncluded = 0.7
    LearningProportion = 0.75
}

let forestReport,forestPredictor = 
    learn forestConfig features sample 

sample
    |> Seq.map (fun (wine,quality) -> 
        quality, forestPredictor wine
        )
    |> Chart.Scatter
    |> Chart.WithOptions options
    |> Chart.Show


let sampleSize = sample |> Seq.length
let trainingSample = sample |> Seq.take (sampleSize/2)
let testingSample = sample |> Seq.skip (sampleSize/2)


[ 10 .. 10 .. 100 ]
|> List.map (fun trees ->
    let config = { forestConfig with Trees = trees }
    let report,predictor = learn config features trainingSample  
    let trainingCost = cost trainingSample predictor
    let testingCost = cost testingSample predictor
    (trees,trainingCost),(trees,testingCost))
|> List.unzip
|> fun (train,test) -> [ train; test ]
|> Chart.Line
|> Chart.WithLabels [ "Training"; "Testing" ]
|> Chart.WithXTitle "# of Trees"
|> Chart.WithYTitle "Cost"
|> Chart.Show