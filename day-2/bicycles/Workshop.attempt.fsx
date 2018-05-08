#I "packages/XPlot.GoogleCharts/lib/net45"
#I "packages/Newtonsoft.Json/lib/net40"
#I "packages/FSharp.Data/lib/net40"
#I "packages/Google.DataTable.Net.Wrapper/lib"
#I "packages/MathNet.Numerics/lib/net40"
#I "packages/MathNet.Numerics.FSharp/lib/net40"

#r "FSharp.Data.dll"
#r "XPlot.GoogleCharts.dll"
#r "Google.DataTable.Net.Wrapper.dll"
#r "Newtonsoft.Json.dll"
#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.FSharp.dll"

open System
open System.IO
open XPlot.GoogleCharts
open FSharp.Data

type Dataset = CsvProvider<"day.csv">
type Datapoint = Dataset.Row
let dataset = Dataset.Load("day.csv")
let data = dataset.Rows

let mostWind = 
    data
    |> Seq.sortByDescending(fun x -> x.Windspeed)
    |> Seq.item 1

let averageRiders = 
    data
    |> Seq.averageBy(fun x -> (float)x.Cnt)

let averageRidersOnHolidays = 
    data
    |> Seq.filter(fun x -> x.Holiday)
    |> Seq.averageBy(fun x -> (float)x.Cnt)

let averageRidersOnSundays = 
    data
    |> Seq.filter(fun x -> x.Dteday.DayOfWeek = DayOfWeek.Sunday)
    |> Seq.averageBy(fun x -> (float)x.Cnt)

// data
//     |> Seq.map(fun x -> x.Dteday, x.Cnt)
//     |> Chart.Line
//     |> Chart.Show

// data
//     |> Seq.map(fun x -> x.Windspeed, x.Cnt)
//     |> Chart.Scatter
//     |> Chart.Show

// data
//     |> Seq.map(fun x -> x.Temp, x.Cnt)
//     |> Chart.Scatter
//     |> Chart.Show

type Model = Datapoint -> float
let prediction (theta:float) (day:Datapoint) = 
    theta * (float day.Instant)

// Chart.Line [
//     (data |> Seq.map (fun day -> day.Instant, float day.Cnt))
//     (data |> Seq.map (fun day -> day.Instant, prediction 1.0 day))
//     (data |> Seq.map (fun day -> day.Instant, prediction 5.0 day))
//     (data |> Seq.map (fun day -> day.Instant, prediction 20.0 day))
//     ]
// |> Chart.WithLabels ["cnt"; "1"; "5"; "20"]
// |> Chart.Show

let absError (model:Model) (day:Datapoint) = 
    abs (model day - (float day.Cnt))

let meanAbsError (model:Model) (data:Datapoint seq) =
    data |> Seq.averageBy (absError model)

let bestTheta = 
    [0.0 .. 1.0 .. 20.0]
    |> Seq.map(fun theta -> (theta, meanAbsError (fun day -> prediction theta day) data))
    |> Seq.minBy(fun x -> snd x)

let complexPrediction (theta0:float,theta1:float,theta2:float) (day:Datapoint) = 
    theta0 + theta1 * (float day.Instant) + theta2 * (float day.Temp)

let complexModel = complexPrediction (500.0, 7.5, 2500.0)
let complexModelError = meanAbsError complexModel data



open MathNet
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

type Vec = Vector<float>
type Mat = Matrix<float>


let predict (theta:Vec) (v:Vec) = theta * v 
let estimate (Y:Vec) (X:Mat) =
    (X.Transpose() * X).Inverse() * X.Transpose() * Y

type Featurizer = Datapoint -> float list

let predictor (f:Featurizer) (theta:Vec) =
    f >> vector >> (*) theta
let evaluate = meanAbsError

let model (f:Featurizer) (data:Datapoint seq) =
    let Yt, Xt = 
        data
        |> Seq.toList
        |> List.map (fun obs -> float obs.Cnt, f obs)
        |> List.unzip
    let theta = estimate (vector Yt) (matrix Xt)
    let predict = predictor f theta
    theta,predict

let train = Dataset.Load("data/train.csv")
let test = Dataset.Load("data/test.csv")

let featurizer0 (obs:Datapoint) = 
    [   1.0; 
        float obs.Instant; ]

let (theta0,model0) = model featurizer0 train.Rows

// Chart.Line [
//     [ for obs in data -> obs.Instant, float obs.Cnt ]
//     [ for obs in data -> obs.Instant, model0 obs ] ]
// |> Chart.Show

evaluate model0 train.Rows |> printfn "Training set: %.0f"
evaluate model0 test.Rows |> printfn "Testing set: %.0f"


// Chart.Scatter [for obs in data -> float obs.Cnt, model0 obs ]
// |> Chart.Show

theta0 |> Seq.iteri (fun i x -> printfn "Coeff. %i: %.1f" i x)

let featurizer1 (obs:Datapoint) = 
    [   1.0 
        float obs.Instant
        //float obs.Temp
        float obs.Windspeed
    ]

let (theta1,model1) = model featurizer1 train.Rows

evaluate model1 test.Rows |> printfn "Testing set: %.0f"


// Chart.Scatter [for obs in data -> float obs.Cnt, model1 obs ]
// |> Chart.Show

let featurizer2 (obs:Datapoint) = 
    [   1.0 
        float obs.Instant
        float obs.Temp
        (if obs.Holiday then 1.0 else 0.0)  ]

let (theta2,model2) = model featurizer2 train.Rows

evaluate model2 test.Rows |> printfn "Testing set: %.0f"

// Chart.Scatter [for obs in data -> float obs.Cnt, model2 obs ]
// |> Chart.Show

theta2 |> Seq.iteri (fun i x -> printfn "Coeff. %i: %.1f" i x)


let featurizer3 (obs:Datapoint) = 
    [   1.0 
        float obs.Instant
        float obs.Windspeed
        float obs.Temp
        float (obs.Temp * obs.Temp)
        float obs.Hum
        (if obs.Holiday then 1.0 else 0.0)
    ]

let (theta3,model3) = model featurizer3 train.Rows
evaluate model3 test.Rows |> printfn "Testing set: %.0f"

Chart.Line [
    [ for obs in data -> obs.Instant, float obs.Cnt ]
    [ for obs in data -> obs.Instant, model3 obs ] ]
|> Chart.Show

Chart.Scatter [for obs in data -> float obs.Cnt, model3 obs ]
|> Chart.Show

// theta3 |> Seq.iteri (fun i x -> printfn "Coeff. %i: %.1f" i x)
