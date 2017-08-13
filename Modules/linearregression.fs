module linearregression

open System
open System.IO
open Deedle
open FSharp.Data
open XPlot.GoogleCharts
open XPlot.GoogleCharts.Deedle
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearRegression
open MathNet.Numerics.Fit

let rows = File.ReadAllLines(@"auto-mpg.csv")
                |> Array.map (fun t -> t.Split(',')
                                        |> Array.map (fun t -> float t))
let mpgData = DenseMatrix.ofRowArrays rows

let myMat = matrix  [[1.0;2.0;3.0]
                     [4.0;5.0;2.0]
                     [7.0;0.8;9.0]]

let qr = myMat.QR()

let x =  [14;16;27;42;39;50;83]
let y =  [02;05;07;09;10;13;20]
let y' = [3;4;5;7;23;21;34]

let calculateCoefficients x y = 
                    let xy = List.zip x y
                                |> List.map (fun it -> float (fst it) * float (snd it))
                                |> List.sum
                    let x_ = x |> List.map (fun z -> float z)
                               |> List.average
                    let y_ = y |> List.map (fun z -> float z)
                               |> List.average
                    let sx_2 = x |> List.sumBy (fun t -> float t * float t)
                    let sy_2 = y |> List.sumBy (fun t -> float t * float t)
                    let n = float x.Length
                    let b1 = (xy - n * x_ * y_)/(sx_2 - n * x_ * x_)
                    let b0 = y_ - b1 * x_
                    (b0, b1)

let coef = calculateCoefficients x y
let b0 = fst coef
let b1 = snd coef

let regressionPairs = x |> List.map ( fun xElem -> (xElem, b0 + b1* float xElem ))

let pairs = List.zip x y