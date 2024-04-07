type id = string
type Expr =
  | None
  | Int of int
  | Float of float
  | String of string
  | Assign of id*Expr*Expr
  | Get of id
  | Func of Expr list
  | Print of string*id*string
  | Call of id*(id*Expr) list


type Env = Map<id, Expr>

let printExpr = function
  | Int(n) -> printf "%d" (n)
  | Float(f) -> printf "%f" (f)
  | String(s) -> printf "%s" (s)
  | other -> failwithf "Error: Print: unresolved type: %A" (other)


let rec eval (expr: Expr) (env: Env) = 
  match expr with
  | None -> None
  | Int(n) -> Int(n)
  | Float(f) -> Float(f)
  | String(s) -> String(s)
  | Assign(id, exprWhat, exprWhere) -> 
     eval exprWhere (Map.add id exprWhat env)
  | Get(id) -> Map.find id env
  | Call(id, args) -> 
    match Map.find id env with
    | Func(commands) ->
      let newEnv = List.fold (fun curEnv (id, exprInner) -> Map.add id (eval exprInner curEnv) curEnv) env args in
      List.fold (fun _ command -> eval command newEnv) None commands
    | other -> failwithf "Eval: unresolved id to Call: %A" (other)
  | Print(before, whatId, after) -> 
    printf "%s" (before) 
    printExpr (eval (Get(whatId)) env)
    printf "%s" (after)
    None

let args = [("age", Int(25)); ("name", String("Vasua"))]
let cmds = [
  Print("Name: ", "name", "!\n");
  Print("Age: ", "age", "\n");
  Assign("sex", (String("man")), Print("Sex: ", "sex", " Yep!\n"));
  Call("func", args)
    ]

let func = Func(cmds)
eval (Assign("func", func, Call("func", args))) Map.empty
    
