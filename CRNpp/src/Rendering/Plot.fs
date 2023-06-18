// Author: Roar Nind Steffensen, 14/06/2023

namespace Rendering

module Plotting =
    open Plotly.NET
    type Plot = P of GenericChart.GenericChart

    let scatter (xs:'a list) (ys:'a list) label= 
        Chart.Scatter(X=xs, Y=ys, Mode=StyleParam.Mode.Lines_Markers, Name=label)
        |> P

    let show title ps = 
        ps 
        |> Seq.map (fun (P(chart)) -> chart)
        |> Chart.combine
        |> Chart.withTitle(Title.init(title))
        |> Chart.show
