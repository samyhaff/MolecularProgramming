// Author: Roar Nind Steffensen, 15/06/2023

module BasicInterperter
open Ast

type Flag = Greater | Equal | Less
type Env = {
    variables : Map<string, float>
    flag: Flag
}

let floatEquals a b =
    abs (b - a) < 1e-5

// NOTE: This applies commands to the env in order. 
//       For parallel application, consider evaluating env changes and adding up to overall env
let rec runCommands env commands =
    List.fold runCommand env commands 

and runCommand env command =
    let setvars v = {env with variables = v}
    let setflag f = {env with flag = f}
    let vars = env.variables
    match command with 
    | Load(a,b)     -> setvars <| Map.add b (Map.find a vars) vars
    | Add(a,b,c)    -> setvars <| Map.add c (Map.find a vars + Map.find b vars) vars
    | Sub(a,b,c)    -> setvars <| Map.add c (Map.find a vars - Map.find b vars) vars
    | Mul(a,b,c)    -> setvars <| Map.add c (Map.find a vars * Map.find b vars) vars
    | Div(a,b,c)    -> setvars <| Map.add c (Map.find a vars / Map.find b vars) vars
    | Sqrt(a,b)     -> setvars <| Map.add b (Map.find a vars |> sqrt) vars
    | Cmp(a,b)      -> setflag <| let va,vb = Map.find a vars, Map.find b vars
                                  in match (va,vb) with 
                                     | _ when floatEquals va vb -> Equal 
                                     | _ when va > vb -> Greater 
                                     | _ -> Less
    | Ifgt(cs)      -> if env.flag = Greater then runCommands env cs else env
    | Ifge(cs)      -> if env.flag <> Less then runCommands env cs else env
    | Iflt(cs)      -> if env.flag = Less then runCommands env cs else env
    | Ifle(cs)      -> if env.flag <> Greater then runCommands env cs else env
    | Ifeq(cs)      -> if env.flag = Equal then runCommands env cs else env

let runStep env commands = 
    runCommands env commands

let run (crn:Crn) = 
    let envVars = crn |> List.fold (fun acc r -> match r with |Step _ -> acc| Conc(s,f) -> (s,f)::acc) [] |> Map.ofList
    let env = {variables = envVars; flag = Equal} // rely in type checker: flag is set before conditional command

    let steps = crn |> List.fold (fun acc r -> match r with |Conc _ -> acc| Step cs -> cs::acc) []
    
    let rec runSteps env steps =
        match steps with
        | []    -> []
        | s::ss -> let env' = runStep env s;
                   in env':: runSteps env' ss

    let rec runner env steps =
        let env' = runSteps env steps
        seq { 
            yield! env'; 
            yield! runner (List.last env') steps
        }

    runner env steps

let envSeqToConc (steps:int) (envSeq: Env seq) =
    let crns = Seq.take steps envSeq |> Seq.toList
    let names = (List.head crns).variables |> Map.keys |> Seq.toList
    let vars = List.map (fun crn -> crn.variables) crns
    List.map ( fun n -> 
        n, List.map (fun var -> Map.find n var) vars
    ) names

