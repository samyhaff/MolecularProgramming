// Date: 11/06/2023
// Contributor(s): Samy

module Config

open FSharp.Configuration
open Color

type Settings = YamlConfig<"Config.yml">
type DesignConfig = { HorizontalSpacing: float; }
type RenderMode =  | Angled | Direct
type RenderConfig = {
    Mode: RenderMode;
    VerticalSpacing: float;
    MaxLinesLabel: int;
    Margin: float;
    Width: int;
    Height: int;
    BackgroundColor: Color;
    LineColor: Color;
    Title: string;
}

let getRenderMode (mode: string) =
    match mode with
    | "Angled" -> Angled
    | "Direct" -> Direct
    | _ -> failwith "Invalid render mode"

let getColor (color: string) =
    match color with
    | "Black" -> Color.Black
    | "White" -> Color.White
    | "Red" -> Color.Red
    | "Green" -> Color.Green
    | "Blue" -> Color.Blue
    | "Yellow" -> Color.Yellow
    | "Magenta" -> Color.Magenta
    | "Cyan" -> Color.Cyan
    | "Orange" -> Color.Orange
    | "Pink" -> Color.Pink
    | "Purple" -> Color.Purple
    | "Brown" -> Color.Brown
    | "Grey" -> Color.Grey
    | "Violet" -> Color.Violet
    | "BlueDark" -> Color.BlueDark
    | "BlueLight" -> Color.BlueLight
    | "CyanDark" -> Color.CyanDark
    | "CyanLight" -> Color.CyanLight
    | "GreyDark" -> Color.GreyDark
    | "GreyLight" -> Color.GreyLight
    | "GreenDark" -> Color.GreenDark
    | "GreenLight" -> Color.GreenLight
    | "OrangeDark" -> Color.OrangeDark
    | "RedDark" -> Color.RedDark
    | _ -> failwith "Invalid color"

let getConfig () =
    {
        HorizontalSpacing = Settings().Designer.HorizontalSpacing;
    },
    {
        Mode = getRenderMode <| Settings().Renderer.Mode;
        VerticalSpacing = Settings().Renderer.VerticalSpacing;
        MaxLinesLabel = Settings().Renderer.MaxLinesLabel;
        Margin = Settings().Renderer.Margin;
        Width = Settings().Renderer.Width;
        Height = Settings().Renderer.Height;
        BackgroundColor = getColor <| Settings().Renderer.Colors.Background;
        LineColor = getColor <| Settings().Renderer.Colors.Line;
        Title = "";
    }
