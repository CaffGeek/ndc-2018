
#r "../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
open FSharp.Data

[<Literal>]
let redWinesPath = @"../data/winequality-red.csv"

type Wines = 
    CsvProvider<
        Sample = redWinesPath,
        Separators = ";",
        Schema = "float,float,float,float,float,float,float,float,float,float,float,float">
// for convenience, we create a type alias
type Wine = Wines.Row
// we can now read the data from the file, as a sequence:
let redWines = Wines.GetSample().Rows
let exampleWine = redWines |> Seq.item 10
printfn "Wine quality: %.2f" exampleWine.Quality
let averageQuality =
    redWines 
    // for each row/wine, extract the quality
    |> Seq.map (fun wine -> wine.Quality)
    // compute the sequence average
    |> Seq.average

let minQuality = 
    redWines
    |> Seq.map (fun wine -> wine.Quality)
    |> Seq.min

let maxQuality = 
    redWines
    |> Seq.map (fun wine -> wine.Quality)
    |> Seq.max

let sampleSize = 
    redWines
    |> Seq.length

#I "../packages/Newtonsoft.Json/lib/net45/"
#I "../packages/Google.DataTable.Net.Wrapper/lib/"
#r "../packages/XPlot.GoogleCharts/lib/net45/XPlot.GoogleCharts.dll"

open XPlot.GoogleCharts

let options = Configuration.Options()
options.dataOpacity <- 0.10

redWines
|> Seq.map (fun wine -> wine.``Citric acid``, wine.Quality)
|> Chart.Scatter
|> Chart.WithOptions options
|> Chart.WithTitle "Citric Acid"
|> Chart.Show

redWines
|> Seq.map (fun wine -> wine.``Volatile acidity``, wine.Quality)
|> Chart.Scatter
|> Chart.WithOptions options
|> Chart.WithTitle "Volatile acidity"
|> Chart.Show