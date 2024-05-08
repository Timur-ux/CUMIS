namespace evaluator

module Evaluator = 
  open AST
  open malloc
  open funcOf
  open sysCalls
  open logger

  type asseccable =
    | Primitive of AST.DataType
    | Callable of AST.Statement

  type addresses_t = Map<int, asseccable>
  type env_t = Map<AST.id, AST.id_t*int>

  type NamesTable = env_t*addresses_t


  let id2Data id (addrTable: addresses_t) (env: env_t) =
    match Map.tryFind id env with
    | Some(_, addr) -> 
      match Map.tryFind addr addrTable with
      | Some(data) -> data
      | None -> failwithf "Error: id %A doesn't have address" (addr)
    | None -> failwithf "Error: using before assigned: %A" (id)

  let rec validate_args expected actual =
    let validate_arg = function
      | (tp, _), AST.Const(c2) ->
        match tp, c2 with
        | "Цел", AST.Int(_) 
        | "Дроб", AST.Float(_)
        | "Строка", AST.String(_)
        | "Булка", AST.Bool(_) -> true
        | _, _ -> false
      | _, _ -> false 
    match expected, actual with
    | h1::t1, h2::t2 -> (validate_arg (h1, h2)) && (validate_args t1 t2)
    | [], [] -> true
    | _, _ -> false

  let rec listPairing lst1 lst2 =
    match lst1, lst2 with
    | h1::t1, h2::t2 -> (h1, h2)::(listPairing t1 t2)
    | [], [] -> []
    | _, _ -> failwithf "Error: listPairing -- list 1 not equal(by length) to list 2"



  let rec eval_AST (ast: list<AST.Statement>) (namesTable: NamesTable): AST.Statement =
      let env, addrTable = namesTable
      let rec eval_expr expr : AST.Expr = 
        match expr with
        | AST.InfixOpExpr(op, a, b) -> 
          let res = FuncOf.funcOf op (eval_expr a) (eval_expr b)
          res
        | AST.Const(c) -> AST.Const(c)
        | AST.Id(i) -> 
          match id2Data i addrTable env with
          | Primitive(data) -> AST.Const(data)
          | Callable(f) -> 
            failwithf "Error: callable object %A cannot be assecced via Id" (f)
        | AST.Call(funcId, args) -> 
          let evaluatedArgs =
            List.map (fun x -> eval_expr x) args
          match funcId with
          | "Печать" ->
            sysCalls.print evaluatedArgs
            AST.Const(AST.Int(0))
          | _ -> 
            let funcData = id2Data funcId addrTable env
            match funcData with
            | Callable(AST.Func(_, expected_args, commands)) ->
              if validate_args expected_args evaluatedArgs then
                let mutable allocated = []
                let newNamesTable =
                  List.fold (fun newNamesTable ((argTp, argId), argVal) ->
                    let env, addrTable = newNamesTable
                    let addr = Memory.malloc()
                    allocated <- addr::allocated
                    match argVal with
                    | AST.Const(value) ->
                      NamesTable(Map.add argId (argTp,addr) env, Map.add addr (Primitive(value)) addrTable)
                    | _ -> failwithf "Error: can't pass non primitive value: %A as function:%A argument" (argVal) (funcId)
                  ) namesTable (listPairing expected_args evaluatedArgs)

                let result = eval_AST commands newNamesTable

                List.fold (fun _ addr ->
                  Memory.free addr
                ) () (allocated)
                match result with
                | AST.Expr(expr) -> eval_expr expr
                | AST.NULL -> AST.Const(AST.Int(0))
                | a -> failwithf "Error: function: %A return undefined value %A" (funcId) (a)
              else
                failwithf "Error: incorrect arguments passed to function: %A\n Expected: %A\nGiven: %A" (funcId) (expected_args) (evaluatedArgs)
            | _ -> failwithf "Error: Not a function to call: %A" (funcId)


      match ast with
      | state::otherStates ->
        Logger.Log (sprintf "Try to eval: %A\n" (state))
        match state with
        | AST.Func(funcId, _, _) ->
          let addr = Memory.malloc()
          eval_AST otherStates ((Map.add funcId ("Пятилетка", addr) env,Map.add addr (Callable(state)) addrTable))         
        | AST.NULL -> eval_AST otherStates namesTable
        | AST.Terminator(_) -> eval_AST otherStates namesTable
        | AST.Expr(expr) ->
          AST.Expr(eval_expr expr) |> ignore
          eval_AST otherStates namesTable
        | AST.Return(expr) -> 
          AST.Expr(eval_expr expr)
        | AST.Assign3(tp, i, expr) ->
          let evaluatedExpr = eval_expr expr
          match evaluatedExpr with
          | AST.Const(data) ->
            if validate_args [(tp, i)] [evaluatedExpr] then
              let addr = Memory.malloc()
              eval_AST otherStates ((Map.add i (tp, addr) env),
                (Map.add addr (Primitive(data)) addrTable))
            else
              failwithf "Error: Pointed type: %A doesn't equal type of expression to assign: %A" (tp) (evaluatedExpr)
          | _ -> failwithf "Error: Can assing only evaluable to primitive data (Цел, Дроб и т.д.). Given: %A" (evaluatedExpr)
        | AST.Assign2(i, expr) ->
          match Map.tryFind i env with
          | Some(tp, addr) -> 
            let evaluatedExpr = eval_expr expr
            match evaluatedExpr with
            | AST.Const(data) ->
              if validate_args [(tp, i)] [evaluatedExpr] then
                eval_AST otherStates (env,
                  (Map.add addr (Primitive(data)) addrTable))
              else
                failwithf "Error: Pointed type: %A doesn't equal type of expression to assign: %A" (tp) (evaluatedExpr)
            | _ -> failwithf "Error: Can assing only evaluable to primitive data (Цел, Дроб и т.д.). Given: %A" (evaluatedExpr)
          | None -> failwithf "Error: using before assigned: %A" (id)
        | AST.If(cond, if_body, else_body_opt) ->
          let evaluatedCond = eval_expr cond
          match evaluatedCond with
          | AST.Const(AST.Bool(res)) ->
            if res then
              eval_AST if_body namesTable
            else
              match else_body_opt with
              | Some(else_body) ->
                eval_AST else_body namesTable
              | None -> 
                eval_AST otherStates namesTable
          | _ -> failwithf "Error: Condition %A doesn't convectible to type Булка" (evaluatedCond)
        | AST.While(cond, while_body) ->
          let evaluatedCond = eval_expr cond
          match evaluatedCond with
          | AST.Const(AST.Bool(res)) ->
            if res then
              eval_AST (while_body@(state::otherStates)) namesTable
            else
              eval_AST otherStates namesTable
          | _ -> failwithf "Error: Condition %A doesn't convectible to type Булка" (evaluatedCond)
      | [] -> AST.NULL
          
