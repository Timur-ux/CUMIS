type id = string
type Expr =
  | ENone
  | Id of id
  | Int of int
  | Float of float
  | String of string
  | False
  | True
  | Op2 of string*Expr*Expr
  | Func of Expr list
  | Assign of id*Expr
  | Get of id
  | Print of string*Expr*string
  | Call of Expr*(id*Expr) list
  | If of Expr*Expr*Expr
  | While of Expr*Expr


type Env(?outEnv: Map<id, Expr>) = 
  let mutable innerEnv = 
    match outEnv with
      | Some env -> env 
      | None -> (Map<id, Expr> [])

  member this.env
    with get() = innerEnv
    and set(outEnv: Map<id, Expr>) = innerEnv <- outEnv

let funcof op =
  match op with
  | "+" -> (fun operand1 operand2 ->
    match operand1, operand2 with
    | (Int(a), Int(b)) -> Int(a+b)
    | (Float(a), Float(b)) -> Float(a + b)
    | (String(a), String(b)) -> String(a + b)
    | (und1, und2) -> failwithf "Uncopatible operands %A and %A, op = +" (und1) (und2))
  | "*" -> (fun operand1 operand2 ->
    match operand1, operand2 with
    | Int(a), Int(b) -> Int(a*b)
    | Float(a), Float(b) -> Float(a*b)
    | (und1, und2) -> failwithf "Uncopatible operands %A and %A, op = *" (und1) (und2))
  | "-" -> (fun operand1 operand2 ->
    match operand1, operand2 with
    | Int(a), Int(b) -> Int(a-b)
    | Float(a), Float(b) -> Float(a-b)
    | (und1, und2) -> failwithf "Uncopatible operands %A and %A, op = -" (und1) (und2))
  | "<=" -> (fun operand1 operand2 ->
    match operand1, operand2 with
    | (Int(a), Int(b)) -> if a <= b then True else False
    | (Float(a), Float(b)) -> if a <= b then True else False
    | (String(a), String(b)) -> if a <= b then True else False
    | (und1, und2) -> failwithf "Uncopatible operands %A and %A, op = <=" (und1) (und2))
  | other -> failwithf "This operator(%A) hasn't realized yet" (other)

let printExpr = function
  | Int(n) -> printf "%d" (n)
  | Float(f) -> printf "%f" (f)
  | String(s) -> printf "%s" (s)
  | ENone -> ()
  | other -> failwithf "Error: Print: unresolved type: %A" (other)

let rec eval (expr: Expr) (env: Env) =
  match expr with
  | ENone -> ENone
  | Id(i) -> Id(i)
  | Int(n) -> Int(n)
  | Float(f) -> Float(f)
  | String(s) -> String(s)
  | False -> False
  | True -> True
  | Func(commands) -> Func(commands)
  | Op2(operator, operand1, operand2) -> eval (funcof operator (eval operand1 env) (eval operand2 env)) env 
  | Get(id) -> Map.find id env.env
  | Assign(id, exprWhat) ->
    env.env <-(Map.add id (eval exprWhat env) env.env)
    Map.find id env.env
  | Call(func, args) ->
    match func with
    | Id(i) ->
      match Map.find i env.env with
      | Func(commands) ->
        let newEnv = List.fold (fun (curEnv: Env) (id, exprInner: Expr) ->
          Env (Map.add id (eval exprInner curEnv) curEnv.env)) env args in
        List.fold (fun _ command -> eval command newEnv) ENone commands
      | other -> failwithf "Eval: unresolved id to Call: %A" (other)
    | Func(commands) ->
      let newEnv = List.fold (fun (curEnv: Env) (id, exprInner: Expr) -> Env (Map.add id (eval exprInner curEnv) curEnv.env)) env args in
      List.fold (fun _ command -> eval command newEnv) ENone commands
    | other -> failwithf "Eval: unresolved type to Call (func or func's id expected): %A" (other)
  | Print(before, whatId, after) ->
    printf "%s" (before)
    printExpr(eval whatId env)
    printf "%s" (after)
    ENone
  | If(condition, ifAction, elseAction) ->
    match eval condition env with
      | True ->
        eval ifAction env
      | False ->
        eval elseAction env
      | _ -> failwithf "Eval: condition's type isn't Bool"
  | While(condition, whileBody) ->
    match eval condition env with
    | True -> eval whileBody env; eval (While(condition, whileBody)) env
    | False -> ENone
    | _ -> failwithf "Eval: while condition's type isn't Bool"

let cmdsFact = [
  Assign("fact", Func([
    Print("n = ", Get("n"), "\n");
    If(Op2("<=", Int(2), Get("n")),
    Op2("*", Get("n"), Call(Id("fact"), [("n", Op2("-", Get("n"), Int(1)))])),
    Int(1));
  ]));
  Call(Id("fact"), [("n", Int(6))]);
]

let cmdsSum = [
  Assign("fact", Func([
    If(Op2("<=", Int(2), Get("n")),
    Op2("*", Get("n"), Call(Id("fact"), [("n", Op2("-", Get("n"), Int(1)))])),
    Int(1));
  ]));
  Assign("sum", Func([
    Print("n = ", Get("n"), "\n");
    If(Op2("<=", Int(2), Get("n")),
    Op2("+", 
      Call(Id("fact"), [ ("n", Get("n"))]),
      Call(Id("sum"), [("n", Op2("-", Get("n"), Int(1)))])),
    Int(1));
  ]));
  Call(Id("sum"), [("n", Int(5))]);
]

let func = Func(cmdsSum)
eval (Call(func, [])) (Env Map.empty)
