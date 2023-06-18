// Date: 11/06/2023
// Contributor(s): Roar

module Color

open Plotly.NET

type Color =
  | Black
  | Blue | BlueDark | BlueLight
  | Brown
  | Cyan | CyanDark | CyanLight
  | Grey | GreyDark | GreyLight
  | Green | GreenDark | GreenLight
  | Magenta
  | Orange | OrangeDark
  | Pink
  | Purple
  | Red | RedDark
  | Violet
  | White
  | Yellow
  | RGB of int * int * int
  | ARGB of int * int * int * int

let mapColor (c:Color) :Plotly.NET.Color =
    match c with
    | Black -> Color.fromKeyword ColorKeyword.Black
    | Blue -> Color.fromKeyword ColorKeyword.Blue
    | BlueDark -> Color.fromKeyword ColorKeyword.DarkBlue
    | BlueLight -> Color.fromKeyword ColorKeyword.LightBlue
    | Brown -> Color.fromKeyword ColorKeyword.Brown
    | Cyan -> Color.fromKeyword ColorKeyword.Cyan
    | CyanDark -> Color.fromKeyword ColorKeyword.DarkCyan
    | CyanLight -> Color.fromKeyword ColorKeyword.LightCyan
    | Grey -> Color.fromKeyword ColorKeyword.Grey
    | GreyDark -> Color.fromKeyword ColorKeyword.DarkGrey
    | GreyLight -> Color.fromKeyword ColorKeyword.LightGrey
    | Green -> Color.fromKeyword ColorKeyword.Green
    | GreenDark -> Color.fromKeyword ColorKeyword.DarkGreen
    | GreenLight -> Color.fromKeyword ColorKeyword.LightGreen
    | Magenta -> Color.fromKeyword ColorKeyword.Magenta
    | Orange -> Color.fromKeyword ColorKeyword.Orange
    | OrangeDark -> Color.fromKeyword ColorKeyword.DarkOrange
    | Pink -> Color.fromKeyword ColorKeyword.Pink
    | Purple -> Color.fromKeyword ColorKeyword.Purple
    | Red -> Color.fromKeyword ColorKeyword.Red
    | RedDark -> Color.fromKeyword ColorKeyword.DarkRed
    | Violet -> Color.fromKeyword ColorKeyword.Violet
    | White -> Color.fromKeyword ColorKeyword.White
    | Yellow -> Color.fromKeyword ColorKeyword.Yellow
    | RGB(r,g,b) -> Color.fromRGB r g b
    | ARGB(a,r,g,b) -> Color.fromARGB a r g b
