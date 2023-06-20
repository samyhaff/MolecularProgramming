// Author: Roar Nind Steffensen, 14/06/2023

namespace Rendering

module Plotting =
    open Plotly.NET
    open Plotly.NET.TraceObjects
    open Plotly.NET.LayoutObjects
    open Plotly.NET.StyleParam
    type Plot = P of GenericChart.GenericChart

    let scatter x y label= 
        Chart.Scatter(X=x, Y=y, Mode=Mode.Lines_Markers, Name=label)
        |> P

    let line x y label=
        Chart.Line (Seq.zip x y, Name=label, ShowMarkers=false, LineWidth=5)
        |> P

    let surface x y z xLabel yLabel zLabel =
        let axis title = LinearAxis.init (Title=Title.init title)

        Chart.Surface (X=x, Y=y, zData=z, Contours=Contours.initXyz(Show=true), ShowScale=false)
        |> Chart.withSceneStyle (XAxis=axis xLabel, YAxis=axis yLabel, ZAxis=axis zLabel, 
                                AspectRatio=AspectRatio.init(1.0, 1.0, 0.5), AspectMode=AspectMode.Manual)
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
        |> Chart.withLayoutStyle (Width=600, Height=800)
        |> Chart.show

    let showLabelledPlots title xLabel yLabel size ps =
        ps 
        |> Seq.map (fun (P(chart)) -> chart)
        |> Chart.combine
        |> Chart.withTitle(Title.init(title))
        |> Chart.withXAxisStyle (xLabel)
        |> Chart.withYAxisStyle (yLabel)
        |> Chart.withLayoutStyle(Width=fst size, Height=snd size)
        |> Chart.show
