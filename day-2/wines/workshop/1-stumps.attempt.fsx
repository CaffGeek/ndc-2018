#r "../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
open FSharp.Data

#I "../packages/Newtonsoft.Json/lib/net45/"
#I "../packages/Google.DataTable.Net.Wrapper/lib/"
#r "../packages/XPlot.GoogleCharts/lib/net45/XPlot.GoogleCharts.dll"

open XPlot.GoogleCharts


[<Literal>]
let redWinesPath = @"../data/winequality-red.csv"

type Wines = 
    CsvProvider<
        Sample = redWinesPath,
        Separators = ";",
        Schema = "float,float,float,float,float,float,float,float,float,float,float,float">

type Wine = Wines.Row

let redWines = Wines.GetSample().Rows

let options = Configuration.Options()
options.dataOpacity <- 0.20
options.pointSize <- 10

let veryBasicPredictor (wine:Wine) = 5.0

let learnAlcoholStump alcoholLevel =
    // average quality for wines with alcohol <= level
    let valueIfLow = 
        redWines
        |> Seq.filter (fun wine -> wine.Alcohol <= alcoholLevel)
        |> Seq.averageBy (fun wine -> wine.Quality)
    // average quality for wines with alcohol > level
    let valueIfHigh =
        redWines
        |> Seq.filter (fun wine -> wine.Alcohol > alcoholLevel)
        |> Seq.averageBy (fun wine -> wine.Quality)
    // create a stump
    let predictor (wine:Wine) =
        if wine.Alcohol <= alcoholLevel
        then valueIfLow
        else valueIfHigh
    // return the stump
    predictor


let alcoholStump1 = learnAlcoholStump 10.0
let alcoholStump2 = learnAlcoholStump 12.0

let actuals = 
    redWines 
    |> Seq.map (fun wine -> wine.Alcohol,wine.Quality)
let predictions1 = 
    redWines 
    |> Seq.map (fun wine -> wine.Alcohol, alcoholStump1 wine)
let predictions2 = 
    redWines 
    |> Seq.map (fun wine -> wine.Alcohol, alcoholStump2 wine)

// [ actuals; predictions1; predictions2 ]
// |> Chart.Scatter
// |> Chart.Show

let calcCost (predictor: Wine -> float) =
    redWines
    |> Seq.averageBy(fun wine ->
        let quality = wine.Quality
        let predicted = predictor wine
        pown (quality - predicted) 2   
    )

let cost0 = calcCost veryBasicPredictor
let cost1 = calcCost alcoholStump1
let cost2 = calcCost alcoholStump2

let levels =
    let values = 
        redWines
        |> Seq.map (fun wine -> wine.Alcohol)    
    let min = values |> Seq.min
    let max = values |> Seq.max
    let width = max - min
    let step = width / 20.0
    [ min + step .. step .. max - step ]

let bestStump =
    levels
    |> Seq.map (fun level -> learnAlcoholStump level)
    |> Seq.minBy (fun stump -> calcCost stump)


let bestCost = 
    redWines 
    |> Seq.averageBy (fun wine -> 
        pown ((wine.Quality)-(bestStump wine)) 2)

redWines
|> Seq.map (fun wine -> wine.Quality, bestStump wine)
|> Chart.Scatter
|> Chart.WithOptions options
|> Chart.WithTitle "Actual vs. Predicted"
|> Chart.WithXTitle "Quality"
|> Chart.WithYTitle "Predicted"
|> Chart.Show