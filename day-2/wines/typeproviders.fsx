#r "./packages/FSharp.Data/lib/net40/FSharp.Data.dll"
open FSharp.Data

type Questions = JsonProvider<"https://api.stackexchange.com/2.2/questions?order=desc&sort=activity&site=stackoverflow">

let questions = Questions.GetSample()

questions.Items
|> Array.map(fun x -> x.Title, x.Owner.DisplayName)
