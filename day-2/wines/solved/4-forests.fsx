(*
FORESTS
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

In this final section, we will combine Trees into Forests.
As we saw in the previous section, when the learning goes
"too deep", Trees over-fit: they match the training data
better and better, but their overall quality degrades.

One way around this is to use Random Forests, which combine
multiple trees together, each using a random selection of
features and of the training data.

Rather than implementing a Random Forest from scratch, we
will use a library, ALGLIB. This will give us a chance to 
see how these libraries typically look like.

The following blog post goes into more detail on how to use
ALGLIB from F#, for various tasks:
http://brandewinder.com/2016/09/25/alglib-random-forest-with-fsharp/
*)

(*
What we have so far
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*)

#load "2-model.fsx"
open ``2-model``

#I "../packages/Newtonsoft.Json/lib/net45/"
#I "../packages/Google.DataTable.Net.Wrapper/lib/"
#r "../packages/XPlot.GoogleCharts/lib/net45/XPlot.GoogleCharts.dll"

open XPlot.GoogleCharts

let options = Configuration.Options()
options.dataOpacity <- 0.20
options.pointSize <- 10



(*
TUTORIAL: 2D ARRAYS
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
If you are familiar with this, you can skip to next topic. 

The library we will use next expects data in 2D arrays.
Let's take a look at some of the tools available in F#.
*)

// The Array2D module contains many useful functions:
let example1 = Array2D.init 3 4 (fun x y -> x + 10 * y)

// the array2D function acts as a constructor:
let example2 = 
    [
        [ 1; 2; 3 ]
        [ 2; 3; 4 ]
    ]
    |> array2D

// accessing data with indexers: 
let exampleValue = example2.[1,2]



(*
Step 1: Using the alglib library
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Rather than implement by hand a random forest, we will use
an existing implementation, from the alglib library:
http://www.alglib.net/dataanalysis/decisionforest.php

In this section, we'll simply demo how to use the library;
then we will look at how our domain model can simplify the
code.
*)

#r @"../packages/alglibnet2/lib/alglibnet2.dll"

(*
The method we are interested in is 
alglib.dfbuildrandomdecisionforestx1.

It takes a bunch of arguments, and returns a tuple:
- info: (unreliable) flags, 
- forest: a forest, which can be used to make predictions,
- report: metrics on the model quality.

let info,forest,report = 
    alglib.dfbuildrandomdecisionforestx1(
        trainingset, // training data
        samplesize, // how many observations
        features, // how many features/variables
        classes, // how many classes; 1 represents regression
        trees, // how many trees to build; recommended: 50 to 100
        featuresincluded, // how many features retained when splitting
        learningproportion // how much of the sample to use for each tree. Recommended: 0.05 (high noise) to 0.66 (low noise)
        )
*)


// Let's start by training a forest using only some of our
// features to start with.

// the input data is expected in 2D array form, with the
// last column containing the value we try to predict:
let trainingset = 
    sample
    |> Seq.map (fun (wine,quality) ->
        [|
            // the inputs x
            wine.Alcohol
            wine.Chlorides
            wine.``Fixed acidity``
            // the output y
            quality
        |])
    |> array2D

// training a forest
let samplesize = trainingset.GetUpperBound(0)
let nFeatures = 3
let classes = 1 // regression
let trees = 10
let featuresincluded = 2
let learningproportion = 0.75
    
let info,forest,report = 
    alglib.dfbuildrandomdecisionforestx1(
        trainingset,
        samplesize,
        nFeatures,
        classes,
        trees,
        featuresincluded,
        learningproportion
        )    

// inspecting quality metrics
report.oobrmserror

// making a prediction: we need to pass an array of floats,
// corresponding to the features, to alglib.dfprocess:
let predict (input:float[]) =
    let mutable result = Array.empty<float>
    alglib.dfprocess(forest, input, &result)
    result.[0]

// to predict the first wine in our sample for instance, we
// would extract the features, and call predict:

let (testWine,testQuality) = sample |> Seq.head

let testWineFeatures = 
    [|
        testWine.Alcohol
        testWine.Chlorides
        testWine.``Fixed acidity``
    |]

let testPrediction = predict testWineFeatures



(*
Step 2: Simplifying predictions
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Using alglib "raw" works, but dealing manually with 2D 
arrays is not very pleasant. If I want to predict the
quality for wine 1, I need to extract manually features,
the same way I built them to train the forest.

This works, but is very error prone, especially if we
start adding more features to our model. Let's see if we
can use our domain to make things a bit smoother.
*)


// first we extract configuration data into a record
type Config = {
    Trees:int // how many trees to train
    FeaturesIncluded:float // % features to keep
    LearningProportion:float // % examples to keep
}

// [TODO] instead of hard-coding the features in an Array 
// we will extract the data preparation in a function
let prepareData (features:Feature list) (sample:Example seq) =
    sample
    |> Seq.map (fun (wine,quality) ->
        [|
            // the inputs x
            for feature in features ->
                // [TODO] extract each feature value
                feature wine
            // the output y
            yield quality
        |])
    |> array2D

// now we wrap the whole learning into a function, simply
// returning the report generated and a predictor:
let learn
        // forest configuration
        (config:Config) 
            // Features we want to use
            (features:Feature list) 
                // Examples we want to learn from
                (sample:Sample) =

    // prepare the trainingset
    let trainingset = prepareData features sample
        
    // learn the forest
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

    // create a Predictor from the forest
    let predict (input:float[]) =
        let mutable result = Array.empty<float>
        alglib.dfprocess(forest, input, &result)
        result.[0]

    let predictor (wine:Wine) =
        features 
        |> List.map (fun feature -> feature wine)
        |> List.toArray
        |> predict
    
    // return the report and the predictor
    report, predictor



(*
Step 3: Using the Forest
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*)

// let's create a default configuration for the forest...
let forestConfig = {
    Trees = 100
    FeaturesIncluded = 0.7
    LearningProportion = 0.75
}

// ... and learn one from the sample:
let forestReport,forestPredictor = 
    learn forestConfig features sample 

// [TODO] plot the actual and predicted qualities that
// our forest produces:

sample
|> Seq.map (fun (wine,quality) -> 
    // [TODO] return quality and predicted quality
    quality, forestPredictor wine)
|> Chart.Scatter
|> Chart.WithOptions options
|> Chart.Show

// How about over-fitting?
let sampleSize = sample |> Seq.length
let trainingSample = sample |> Seq.take (sampleSize/2)
let testingSample = sample |> Seq.skip (sampleSize/2)

// [TODO]: draw the error on training and testing sample,
// for forests of size 10, 20, ... 100:

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
|> Chart.Show

