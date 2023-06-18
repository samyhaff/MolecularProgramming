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

    let surface x y z xLabel yLabel zLabel =
        let axis title scaleRatio scaleAnchor id = LinearAxis.init (
                Title=Title.init title, 
                ScaleRatio=scaleRatio, 
                ScaleAnchor=scaleAnchor,
                Overlaying = id
            )

        let free = LinearAxisId.Free
        let xId = LinearAxisId.X 0
        let yId = LinearAxisId.Y 0

        let xAxis = axis xLabel 1.0 free xId
        let yAxis = axis yLabel 1.0 free yId
        let zAxis = axis zLabel 0.5 xId free

        Chart.Surface (X=x, Y=y, zData=z, Contours=Contours.initXyz(Show=true), ShowScale=false)
        |> Chart.withSceneStyle (XAxis=xAxis, YAxis=yAxis, ZAxis=zAxis, 
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
        |> Chart.show
