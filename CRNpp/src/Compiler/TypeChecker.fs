module TypeChecker

open Ast

type Flag = UnSet | Set
type Env = {
   variables: Set<string>;
   flag: Flag
}

let rec checkModule (command: Command) (env: Env)=
   match command with
   | Add(s1, s2, s3)
   | Sub(s1, s2, s3)
   | Mul(s1, s2, s3)
   | Div(s1, s2, s3) ->
      (s3 <> s1 && s3 <> s2 && Set.contains s1 env.variables && Set.contains s2 env.variables,
      {env with variables = Set.add s3 env.variables})
   | Load(s1, s2)
   | Sqrt(s1, s2) ->
      (s1 <> s2 && Set.contains s1 env.variables, {env with variables = Set.add s2 env.variables})
   | Cmp(s1, s2) ->
      (s1 <> s2 && Set.contains s1 env.variables && Set.contains s2 env.variables,
      {env with flag = Set})
   | Iflt(commands)
   | Ifle(commands)
   | Ifeq(commands)
   | Ifge(commands)
   | Ifeq(commands)
   | Ifgt(commands) ->
      let (isValid, env') = checkModules commands env
      (env.flag = Set && isValid, env')
and checkModules (commands: Command list) (env: Env) =
   let rec aux commands newEnv =
      match commands with
      | [] -> (true, newEnv)
      | command :: rest ->
         let (isValid, env') = checkModule command env
         let res = aux rest {newEnv with flag = env'.flag; variables = Set.union newEnv.variables env'.variables}
         (isValid && fst res, env')
   aux commands env

let checkProgram (program: Crn) =
   let rec aux program env =
      match program with
      | [] -> (true, env)
      | Conc(s, _) :: rest -> aux rest { env with variables = Set.add s env.variables }
      | Step(commands) :: rest ->
         let (isValid, env') = checkModules commands env
         let (isValidRest, env'') = aux rest env'
         (isValid && isValidRest, env'')
   aux program {variables = Set.empty; flag = UnSet}