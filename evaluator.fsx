type Id = string

type Expr =
  | Assignation of Id*Expr*Expr
  | Getting of Id
  | Int of int
  | Float of float
  | String of string

type Environment = Map<Id, Expr>

let func = function 
  | "+" -> (function 
    | Int(a), Int(b) -> Int(a+b)
    | Float(a), Float(b) -> Float(a+b)
    | String(a), String(b) -> String(a+b))

func "+" (Float(1.2), Float(2.3))
func "+" (Int(1), Int(2))
func "+" (String("abs"), String("cvf"))

let rec evaluate expr (env: Environment) =
  match expr with
    | Int(n) -> Int(n)
    | Float(f) -> Float(f)
    | String(str) -> String(str)
    | Getting(id) -> Map.find id env 
    | Assignation(id, what, toWhat) -> 
      let data = evaluate what env in evaluate toWhat (Map.add id data env)


let expr1 = Assignation("bullet", Int(5), Getting("bullet"))

evaluate expr1 Map.empty
