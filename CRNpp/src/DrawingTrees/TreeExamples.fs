// Date: 11/06/2023
// Contributor(s): Roar, Samy

module TreeExamples
open System
open Config
open TreeDesigner
open TreeRenderer

let rnd = new Random()
open TreeDesigner
let singleNode =
    Node("root", [])

let rec binaryTree height =
    let height = max height 1
    match height with
    | 1 -> Node(1, [])
    | n -> Node(n, [binaryTree (n-1); binaryTree (n-1)])

let parallelPaths paths height =
    let height = max height 1
    let rec pathOfLength = function | 0 -> Node(0, []) | n -> Node(n, [pathOfLength (n-1)])
    Node(height, seq {1..paths} |> Seq.map (fun _ -> pathOfLength (height-1)) |> Seq.toList)

let configExamples () =
    // example 1
    let designerConfig, rendererConfig = getConfig()
    let tree = Node(
        "line 1\nline 2",
        [
            Node("left child", [Node("left child", []); Node("middle child", []); Node("right child", [])])
            Node("right child", [Node("left child", []); Node("right child", [])]);
        ]
    )
    design designerConfig tree
    |> render { rendererConfig with Title = "Example 1: Default config" }

    // example 2
    let tree = Node(
        "line 1\nline 2",
        [
            Node("left\nchild", [Node("left\nchild", []); Node("middle\nchild", []); Node("right\nchild", [])])
            Node("right\nchild", [Node("left\nchild", []); Node("right\nchild", [])]);
        ]
    )
    design designerConfig tree
    |> render { rendererConfig with Title = "Example 2: Multiple lines labels"; MaxLinesLabel = 2 }

    // example 3
    let designerConfig = { HorizontalSpacing = 1.0 }
    let rendererConfig = {
            Mode = Direct;
            VerticalSpacing = 2.0;
            MaxLinesLabel = 2;
            Margin = 20.0;
            Width = 800;
            Height = 800;
            BackgroundColor = getColor "GreyDark";
            LineColor = getColor "White";
            Title = "Example 3: Different drawing style";
        }
    let tree = Node(
        "line 1\nline 2",
        [
            Node("left\nchild", [Node("left\nchild", []); Node("middle\nchild", []); Node("right\nchild", [])])
            Node("right\nchild", [Node("left\nchild", []); Node("right\nchild", [])]);
        ]
    )
    design designerConfig tree
    |> render rendererConfig
