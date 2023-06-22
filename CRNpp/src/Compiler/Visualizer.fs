// Author: Samy Haffoudhi, 21/06/2023

module Visualizer

open Ast
open TreeDesigner
open TreeRenderer

let rec convert (ast: Crn) : Tree<string> =
   let rec rootConvert roots =
      match roots with
      | [] -> []
      | Conc(s, c)::rest -> Node($"Conc({s}, {c})", [])::(rootConvert rest)
      | Step(commands)::rest ->
         let rec commandConvert commands =
            match commands with
            | []  -> []
            | Rxn(reactants, products, rate)::rest ->
               let name (S s) = s
               let sep = " + "
               Node(
                  $"Rxn({String.concat sep (List.map name reactants)},
                  {String.concat sep (List.map name products)},
                  {rate})", []
               )::(commandConvert rest)
            | Load(s1, s2)::rest -> Node($"Load({s1}, {s2})", [])::(commandConvert rest)
            | Add(s1, s2, s3)::rest -> Node($"Add({s1}, {s2}, {s3})", [])::(commandConvert rest)
            | Sub(s1, s2, s3)::rest -> Node($"Sub({s1}, {s2}, {s3})", [])::(commandConvert rest)
            | Mul(s1, s2, s3)::rest -> Node($"Mul({s1}, {s2}, {s3})", [])::(commandConvert rest)
            | Div(s1, s2, s3)::rest -> Node($"Div({s1}, {s2}, {s3})", [])::(commandConvert rest)
            | Sqrt(s1, s2)::rest -> Node($"Sqrt({s1}, {s2})", [])::(commandConvert rest)
            | Cmp(s1, s2)::rest -> Node($"Cmp({s1}, {s2})", [])::(commandConvert rest)
            | Ifgt(commands)::rest -> Node($"Ifgt", commandConvert commands)::(commandConvert rest)
            | Ifge(commands)::rest -> Node($"Ifge", commandConvert commands)::(commandConvert rest)
            | Iflt(commands)::rest -> Node($"Iflt", commandConvert commands)::(commandConvert rest)
            | Ifle(commands)::rest -> Node($"Ifle", commandConvert commands)::(commandConvert rest)
            | Ifeq(commands)::rest -> Node($"Ifeq", commandConvert commands)::(commandConvert rest)
         Node("Step", commandConvert commands)::(rootConvert rest)
   in Node("Crn", rootConvert ast)

let drawAst (ast: Crn) =
   let designConfig, renderConfig = Config.getConfig()
   ast |> convert |> design designConfig |> render {renderConfig with Width=1200}
