namespace ExprParser

module ExprParser =
  open AST
  open FParsec
  open BaseParsers

  type Parser<'t> = Parser<'t, unit>

  let ws = spaces
  let str s : Parser<string> = ws >>. pstring s .>> ws 

  
  let const_p: Parser<AST.Expr> = choice (seq {for KeyValue(_, v) in Base.const_pDict -> v |>> AST.Const})
  
  let opp = OperatorPrecedenceParser<AST.Expr, unit, unit>()
  let expr_p = opp.ExpressionParser
  
  let call_p = 
    tuple2
      Base.var_p
      (between (str "(") (str ")") (sepBy expr_p (str ",")))
    |>> AST.Call

  let term =
    const_p <|>
    (attempt call_p) <|>
    Base.id_p <|>
    between (str "(") (str ")") expr_p

  opp.TermParser <- term

  type Assoc = Associativity

  type dt = AST.DataType

  opp.AddOperator(InfixOperator("*", ws, 20, Assoc.Left, (fun x y -> AST.InfixOpExpr("*", x, y))))
  opp.AddOperator(InfixOperator("/", ws, 20, Assoc.Left, (fun x y -> AST.InfixOpExpr("/", x, y))))

  opp.AddOperator(InfixOperator("+", ws, 10, Assoc.Left, (fun x y -> AST.InfixOpExpr("+", x, y))))
  opp.AddOperator(InfixOperator("-", ws, 10, Assoc.Left, (fun x y -> AST.InfixOpExpr("-", x, y))))

  opp.AddOperator(InfixOperator("<", ws, 8, Assoc.Left, (fun x y -> AST.InfixOpExpr("<", x, y))))
  opp.AddOperator(InfixOperator("<=", ws, 8, Assoc.Left, (fun x y -> AST.InfixOpExpr("<=", x, y))))
  opp.AddOperator(InfixOperator(">", ws, 8, Assoc.Left, (fun x y -> AST.InfixOpExpr(">", x, y))))
  opp.AddOperator(InfixOperator(">=", ws, 8, Assoc.Left, (fun x y -> AST.InfixOpExpr(">=", x, y))))

  opp.AddOperator(InfixOperator("==", ws, 6, Assoc.Left, (fun x y -> AST.InfixOpExpr("==", x, y))))
  opp.AddOperator(InfixOperator("!=", ws, 6, Assoc.Left, (fun x y -> AST.InfixOpExpr("!=", x, y))))

