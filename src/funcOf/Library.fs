namespace funcOf

module FuncOf =
  open AST

  let unsupportedError op a b =
    failwithf "Error: unsupported operation: %A for operands %A and %A" (op) (a) (b)


  let opArifmetic op (aCnst: AST.Expr) (bCnst: AST.Expr) =
    match aCnst, bCnst with
    | AST.Const(AST.Int(a)), AST.Const(AST.Int(b)) ->
      match op with
      | "+" -> AST.Int(a + b)
      | "-" -> AST.Int(a - b)
      | "*" -> AST.Int(a * b)
      | "/" -> AST.Int(a / b)
      | _ -> unsupportedError op aCnst bCnst
    | AST.Const(AST.Float(a)), AST.Const(AST.Float(b)) ->
      match op with
      | "+" -> AST.Float(a + b)
      | "-" -> AST.Float(a - b)
      | "*" -> AST.Float(a * b)
      | "/" -> AST.Float(a / b)
      | _ -> unsupportedError op aCnst bCnst
    | _ -> unsupportedError op aCnst bCnst

  let opBoolOfFloat op (a: float) (b: float) =
    match op with
    | "<" ->  AST.Bool(a < b)
    | ">" ->  AST.Bool(a > b)
    | "<=" -> AST.Bool(a <= b)
    | ">=" -> AST.Bool(a >= b)
    | "==" -> AST.Bool(a = b)
    | "!=" -> AST.Bool(a <> b)
    | _ -> unsupportedError op a b

  let funcOf op (a: AST.Expr) (b: AST.Expr) =
    match a, b with
    | AST.Const(aVal), AST.Const(bVal) ->
      match aVal, bVal with
      | AST.Int(aInt), AST.Int(bInt) ->
        match op with
        | "+"
        | "-"
        | "*"
        | "/" -> AST.Const(opArifmetic op a b)
        | _ -> AST.Const(opBoolOfFloat op aInt bInt)
      | AST.Float(aFloat), AST.Float(bFloat) ->
        match op with
        | "+"
        | "-"
        | "*"
        | "/" -> AST.Const(opArifmetic op a b)
        | _ -> AST.Const(opBoolOfFloat op aFloat bFloat)
      | _, _ -> unsupportedError op a b
    | _, _ -> unsupportedError op a b
