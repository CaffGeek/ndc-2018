#load "FsLab.fsx"
open FsLab
open System
open System.IO
open XPlot.GoogleCharts

let featuresFile = Path.Combine(__SOURCE_DIRECTORY__, "features.txt")
let features = File.ReadAllLines(featuresFile)

let getFeatureVector text = 
    let counts = 
        text
        |> Seq.pairwise
        |> Seq.map (fun (c1, c2) -> string c1 + string c2)
        |> Seq.countBy(fun x -> x)

    let countLookup = dict counts
    let total = float (String.length text - 1)

    features
    |> Array.map(fun x -> if countLookup.ContainsKey(x) then (float)countLookup.[x]/total else 1e-10) 

let cleanDir = Path.Combine(__SOURCE_DIRECTORY__, "clean")
let languageFeatures = 
    Directory.GetFiles(cleanDir, "*.txt")
    |> Array.Parallel.map (fun file ->
        Path.GetFileNameWithoutExtension(file),
        getFeatureVector (File.ReadAllText(file)) )

let byLanguage = dict languageFeatures

#load "KNN.fsx"
open KNN.NearestNeighbor

type Prediction = { Language: string; Distance: float }
let classifyLanguage text =
    let textFV = getFeatureVector text
    let prediction = 
        byLanguage
        |> Seq.map(fun x -> { Language = x.Key; Distance = distance x.Value textFV })
        |> Seq.sortBy(fun x -> x.Distance)
        |> Seq.head 
    prediction.Language

let prediction1 = classifyLanguage "tohle je nejaky text napsany v ceskem jazyce"
let prediction2 = classifyLanguage "the recent expose of amazons aggressive corporate culture was a cause of consternation to many but plenty of others couldnt see what the problem was"
let prediction3 = classifyLanguage "Negotiators paving the way for a global climate change agreement in Paris have cleared a major hurdle, producing a draft accord in record time and raising hopes that a full week of minister-led talks can now clinch a deal despite many sticking points."
let prediction4 = classifyLanguage "Unbequeme Politiker werden ermordet, Wahlkreise radikal umgestellt, wie es der sozialistischen Regierung passt: Trotzdem steht in Venezuela die Opposition vor dem Wahlsieg."
let prediction5 = classifyLanguage "us stock markets follow global plunge as china concerns deepen"

