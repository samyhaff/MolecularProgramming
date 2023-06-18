// Date: 11/06/2023
// Contributor(s): Roar, Samy

module TreeRenderer

open TreeDesigner
open Renderer
open Config

let getPositions trees =
    List.map (fun (Node((_,p),_)) -> p) trees

let parseLabel config lbl =
    let maxLines = config.MaxLinesLabel
    let maxLineLength = 10

    let takeMax limit list =
        List.take (min limit (List.length list)) list

    string lbl
        |> StringF.replace "\n" "<br>"
        |> StringF.split "<br>"
        |> List.filter StringF.notWhitespace
        |> takeMax maxLines
        |> List.map (StringF.truncate maxLineLength)

let getRendering config tree =
    let rec helper level (xOffset:Position) (Node((label, position), subtrees)) =
        let lineColor = config.LineColor

        let nodeX = position + xOffset
        let nodePoint = point config (nodeX, level) (parseLabel config label)
        let subTreePositions = List.map (fun (Node((_,p),_)) -> p + nodeX) subtrees
        if config.Mode = Direct then
            let subTreeConnections = List.map (fun p -> line (nodeX, level) (p, level- config.VerticalSpacing) lineColor) subTreePositions
            let nodeChart = subTreeConnections @ [nodePoint] |> combineRenderings
            let subCharts = List.map (helper (level - config.VerticalSpacing) nodeX) subtrees
            nodeChart :: subCharts |> combineRenderings
        else
            let subTreeHorizontalConnections =
                if List.isEmpty subtrees then []
                else [line (List.min subTreePositions, level - config.VerticalSpacing / 5.0) (List.max subTreePositions, level - config.VerticalSpacing / 5.0) lineColor]
            let subTreeVerticalConnections = List.map (fun p -> line (p, level - config.VerticalSpacing / 5.0) (p, level - config.VerticalSpacing) lineColor) subTreePositions
            let nodeSubTreeConnection =
                if List.isEmpty subtrees then []
                 else [line (nodeX, level) (nodeX, level - config.VerticalSpacing / 5.0) lineColor]
            let nodeChart = subTreeHorizontalConnections @ nodeSubTreeConnection @ subTreeVerticalConnections @ [nodePoint] |> combineRenderings
            let subCharts = List.map (helper (level - config.VerticalSpacing) nodeX) subtrees
            nodeChart :: subCharts |> combineRenderings
    helper 0 0.0 tree

let getBounds (config: RenderConfig) (tree:Tree<'a*Position>) =
    let verticalSpacing = config.VerticalSpacing
    let rec maxBounds ypos (Node((_, p),c)) =
        let childBounds = List.map (maxBounds (ypos-verticalSpacing)) c
        let (xBounds, yBounds) = List.unzip childBounds

        // adding x pos to child positions to translate relative positions
        let xmin' = List.map fst xBounds |> List.map ((+)p) |> List.fold min p
        let xmax' = List.map snd xBounds |> List.map ((+)p) |> List.fold max p
        let ymin' = List.map fst yBounds |> List.fold min ypos
        let ymax' = List.map snd yBounds |> List.fold max ypos
        (xmin', xmax'), (ymin', ymax')
    in maxBounds 0 tree

let render (config: RenderConfig) (tree:Tree<'a * Position>) =
    let margin = config.Margin

    let marginScale = margin / 100.0
    let ((xmin,xmax),(ymin,ymax)) = getBounds config tree
    let xRange = xmax - xmin
    let yRange = ymax - ymin
    let yMinMax = (ymin - yRange * marginScale, ymax + yRange * marginScale)
    let xMinMax = (xmin - xRange * marginScale, xmax + xRange * marginScale)

    printfn $"bounds: {(xmin,xmax)} {(ymin,ymax)}. yminmax: {yMinMax}. xminmax: {xMinMax}"
    getRendering config tree |> plot config (xMinMax, yMinMax)
