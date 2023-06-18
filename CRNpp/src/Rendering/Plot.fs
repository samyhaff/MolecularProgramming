// Author: Roar Nind Steffensen, 14/06/2023

namespace Rendering

module Plotting =
    open Plotly.NET
    open Plotly.NET.TraceObjects
    type Plot = P of GenericChart.GenericChart

    type DataPoints = float list
    let scatter (x:DataPoints) (y:DataPoints) label= 
        Chart.Scatter(X=x, Y=y, Mode=StyleParam.Mode.Lines_Markers, Name=label)
        |> P

    let surface (x:DataPoints) (y:DataPoints) (z:DataPoints list) xLabel yLabel zLabel =
        Chart.Surface(X=x, Y=y, zData=z, Contours=Contours.initXyz(Show=true), ShowScale=false)
        |> Chart.withXAxisStyle (xLabel, ShowGrid=false, ShowLine=false)
        |> Chart.withYAxisStyle (yLabel, ShowGrid=false, ShowLine=false)
        |> Chart.withZAxisStyle (zLabel, ShowGrid=false, ShowLine=false)
        |> P

    let showPlot title (P(chart)) =
        chart
        |> Chart.withTitle(Title.init(title))
        |> Chart.show

    let showPlots title ps = 
        ps 
        |> Seq.map (fun (P(chart)) -> chart)
        |> Chart.combine
        |> Chart.withTitle(Title.init(title))
        |> Chart.show
