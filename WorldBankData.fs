module WorldData

open Deedle
open FSharp.Data
open XPlot.GoogleCharts
open XPlot.GoogleCharts.Deedle

// Connect to the WorldBank and access indicators EU and CZ
// Try changing the code to look at stats for your country!
let wb = WorldBankData.GetDataContext()
let cz = wb.Countries.``Czech Republic``.Indicators
let eu = wb.Countries.``European Union``.Indicators

// Use Deedle to get time-series with school enrollment data
let czschool = series cz.``Gross enrolment ratio, tertiary, both sexes (%)``
let euschool = series eu.``Gross enrolment ratio, tertiary, both sexes (%)``

// Get 5 years with the largest difference between EU and CZ
let result = abs (czschool - euschool)
             |> Series.sort
             |> Series.rev
             |> Series.take 5

// Plot a line chart comparing the two data sets 
// (Opens a web browser window with the chart)
let chart = [ czschool.[1975 .. 2010]; euschool.[1975 .. 2010] ]
            |> Chart.Line
            |> Chart.WithOptions (Options(legend=Legend(position="bottom")))
            |> Chart.WithLabels ["CZ"; "EU"]
            
let euroGDP = 
 [ let euro = wb.Regions.``Euro area``
   yield "EU","",euro.Indicators.``GDP (current US$)``.[2010] 
   for c in euro.Countries do
     yield c.Name,"EU",c.Indicators.``GDP (current US$)``.[2010]]
     

let treemap = Chart.Treemap(euroGDP)
              |> Chart.WithOptions (Options(minColor="#B24590", midColor="#449AB5", maxColor="#76B747", headerHeight=0, showScale=true))
              