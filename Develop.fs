open System
open System.IO
open Deedle
open FSharp.Data
open XPlot.GoogleCharts
open XPlot.GoogleCharts.Deedle

type Entry = {Label: string; Values: int list}

// Calculates Squared Euclidean distance between pixels values of two images
let distance (values1: int list, values2: int list) =
    values1
    |> List.zip values2
    |> List.map (fun it -> Math.Pow(float(fst it) - float(snd it), 2.0))
    |> List.sum
    
// Loading valies from training/test data. This assumes that the first one is the
// label/class/category of the data
let loadValues (filename: String) =
    File.ReadAllLines(filename)
        |> Seq.ofArray
        // skip header
        |> Seq.skip (1)
        |> Seq.map (fun line ->
                {
                    Label = line.Substring(0, line.IndexOf(','));
                    Values = line.Split(',')
                                |> Seq.ofArray
                                // skip label
                                |> Seq.skip (1)
                                |> Seq.map (fun n -> Convert.ToInt32(n))
                                |> Seq.toList
                })
        |> Seq.toList

let knn (entries: Entry list, newEntry: string * int[], k: int) =
    entries |> List.map (fun x -> (x.Label, distance (x.Values, snd (newEntry)
                                                                 |> Array.toList)))
    |> List.sortBy (fun x -> snd x)
    |> Seq.ofList
    |> Seq.take k
    |> Seq.countBy (fun x -> fst x)
    |> Seq.toList


let loaded = loadValues @"digit-recognizer.csv"

[<EntryPoint>]
let maing argv =
    printfn "%A" loaded.Length
    0























