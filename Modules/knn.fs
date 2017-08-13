module knn

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
    entries |> List.map (fun x -> (x.Label, distance (x.Values, snd (newEntry) |> Array.toList)))
    |> List.sortBy (fun x -> snd x)
    |> Seq.ofList
    |> Seq.take k
    |> Seq.countBy (fun x -> fst x)
    |> Seq.toList


// location of the csv training file
let loaded = loadValues @"digit-recognizer.csv"

// this entry is '5' from the training set
let newEntry = ("X",[|0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;41;149;156;179;254;254;201;119;46;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;13;147;241;253;253;254;253;253;253;253;245;160;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;31;224;253;253;180;174;175;174;174;174;174;223;247;145;6;0;0;0;0;0;0;0;0;0;0;0;0;7;197;254;253;165;2;0;0;0;0;0;0;12;102;184;16;0;0;0;0;0;0;0;0;0;0;0;0;152;253;254;162;18;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;235;254;158;15;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;74;250;253;15;0;0;0;16;20;19;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;7;199;253;253;0;0;25;130;235;254;247;145;6;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;20;253;253;177;100;219;240;253;253;254;253;253;125;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;5;193;253;253;254;253;253;200;155;155;238;253;229;23;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;61;249;254;241;150;30;0;0;0;215;254;254;58;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;36;39;30;0;0;0;0;0;214;253;234;31;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;41;241;253;183;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;201;253;253;102;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;114;254;253;154;5;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;62;254;255;241;30;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;10;118;235;253;249;103;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;55;81;0;102;211;253;253;253;135;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;79;243;234;254;253;253;216;117;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;48;245;253;254;207;126;27;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0|])
// 5 nearest neigbors
let k = 5
// getting back the labels for each of the neighbors
let labels = knn (loaded, newEntry, k)
// locating the guess. the one with the maximum votes
let guess = fst (labels |> List.item 0)




















